{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Foreign.CUDA.Magma.TH where
import Control.Applicative
import Control.Arrow
import Control.Monad ((>=>), join, void)

import GHC.Exts (groupWith)

import Language.Haskell.TH as TH
import Language.C
import Language.C.System.GCC

import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import Data.Char (toLower, toUpper)
import Data.Maybe (mapMaybe)

import Debug.Trace
import qualified Foreign.C.Types as C
import qualified Foreign.C.String as C (castCharToCChar, castCCharToChar)
import qualified Foreign as F
import Foreign.Storable.Complex ()
import Data.Complex (Complex(..))

import Data.Either (partitionEithers)
import Data.Maybe (isJust)

import System.FilePath.Posix ((</>))

import qualified Text.Parsec as P

import Foreign.CUDA as FC
import qualified Foreign.CUDA.Runtime.Stream as FC

import Foreign.CUDA.Magma.Types

try :: [(Bool, a)] -> a
try ((p,y):conds) = if p then y else try conds
try [] = error "TH.try: No match!"

data TypeInfo = TI 
  { ctype    :: Q Type
  , hsinput  :: Either Convert Create
  , hsoutput :: Maybe (Either Convert Destroy) }

data TypeDat = TD 
  { ct    :: Q Type
  , hst   :: Q Type 
  , c2hs  :: (Q Exp, ExpType)
  , hs2c  :: (Q Exp, ExpType) }

data ExpType = Pure | Monadic

data TypeC = VoidC | IntC | CharC | FloatC | DoubleC | EnumC String 
           | ComplexC TypeC | ArbStructC String | PtrC TypeC | HPtrC TypeC | PhonyC TH.Name
           | ArrC TypeC
           deriving (Eq, Show)

prim :: Q Type -> Q Type -> Q Exp -> Q Exp -> TypeDat
prim ct hst c2hs hs2c = TD ct hst (c2hs, Pure) (hs2c, Pure)

simple :: Q Type -> TypeDat
simple t = prim t t [| id |] [| id |]

bothc :: (a -> b) -> Complex a -> Complex b
bothc f (a :+ b) = f a :+ f b 

typeDat :: TypeC -> TypeDat
typeDat (PhonyC n)  = simple (varT n)
typeDat VoidC       = simple [t| () |] 
typeDat (PtrC t)    = prim [t| F.Ptr $(ctype) |] [t| FC.DevicePtr $(ctype) |] [| FC.DevicePtr |]       [| FC.useDevicePtr |] where
  ctype = ct (typeDat t)
typeDat (HPtrC t)    = simple [t| F.Ptr $(ctype) |] where ctype = ct (typeDat t)
typeDat (ArrC t)    = typeDat (PtrC t)
typeDat IntC        = prim [t| C.CInt |]    [t| Int |]    [| fromIntegral |]          [| fromIntegral |]
typeDat CharC       = prim [t| C.CChar |]    [t| Char |]   [| C.castCCharToChar |]       [| C.castCharToCChar |]
typeDat FloatC      = simple [t| C.CFloat |]
typeDat DoubleC     = simple [t| C.CDouble |]
typeDat (EnumC str) = prim [t| C.CInt |] x [| toEnum . fromIntegral |] [| fromIntegral . fromEnum |] where
  x = case str of
     "magma_uplo_t" -> [t| UpLoT |]
     "magma_trans_t" -> [t| TransT |]
     "magma_vec_t" -> [t| VecT |]
     _ -> error ("typeDat.EnumC : Missing type: " ++ str)
typeDat (ArbStructC str) = error "typeDat.ArbstructC"
typeDat (ComplexC t) = prim
  [t| Complex $(ctype) |]
  [t| Complex $(hstype) |]
  [| bothc $(fromC) |]
  [| bothc $(toC) |]
  where
  TD ctype hstype (fromC, Pure) (toC, Pure) = typeDat t


convertT x y = Left (Convert x y)
createT = Right . Create
destroyT = Right . Destroy

data Convert = Convert (Q Type) (Q Exp)
newtype Create = Create (Q Exp)
newtype Destroy = Destroy (Q Exp)

pointerify :: Q Type -> Q Type
pointerify x = [t| F.Ptr $(x) |]

useT :: TypeC -> TypeInfo
useT = useT' . typeDat where
  useT' (TD ct hst c2hs (hs2c,purity)) = TI
    ct
    (convertT hst exp)
    Nothing
    where
    exp = case purity of Pure -> [| return . $(hs2c) |]; Monadic -> hs2c

inT :: TypeC -> TypeInfo
inT (PtrC t) = inT' (typeDat t) where
  inT' (TD ct hst c2hs (hs2c,purity)) = TI
    (pointerify ct)
    (convertT hst exp)
    (Just (destroyT [| F.free |]))
    where
    exp = case purity of Pure -> [| F.new . $(hs2c) |] ; Monadic -> undefined
inT (ArrC t) = inT' (typeDat t) where
  inT' (TD ct hst c2hs (hs2c,purity)) = TI
    (pointerify ct)
    (convertT [t| [ $(hst) ] |] exp)
    (Just (destroyT [| F.free |]))
    where
    exp = case purity of Pure -> [| F.newArray . map $(hs2c) |] ; Monadic -> undefined

outT :: TypeC -> TypeInfo
outT (PtrC t) = outT' (typeDat t) where
  outT' (TD ct hst (c2hs,purity) hs2c) = TI
    ct
    (createT [| F.malloc |])
    (Just (convertT hst [| \p -> do { x <- F.peek p ; F.free p; $(exp) x } |]))
    where
    exp = case purity of Pure -> [| return . $(c2hs) |] ; Monadic -> c2hs

inOutT :: TypeC -> TypeInfo
inOutT (PtrC t) = inOutT' (typeDat t) where
  inOutT' (TD ct hst (c2hs,purity1) (hs2c,purity2)) = TI
    (pointerify ct)
    (convertT hst exp1)
    (Just (convertT hst [| \p -> do { x <- F.peek p ; F.free p; $(exp2) x } |]))
    where
    exp1 = case purity1 of Pure -> [| F.new . $(hs2c) |] ; Monadic -> hs2c
    exp2 = case purity2 of Pure -> [| return . $(c2hs) |] ; Monadic -> c2hs

convert :: CTypeSpecifier a -> TypeC
convert (CVoidType _) = VoidC
convert (CCharType _) = CharC
--CCharType a	 
--CShortType a	 
convert (CIntType _) = IntC
--CLongType a	 
convert (CFloatType _) = FloatC
convert (CDoubleType _) = DoubleC
--CSignedType a	 
--CUnsigType a	 
--CBoolType a	 
--CComplexType a
convert (CTypeDef ident _) = try 
  [ (s=="magma_int_t", IntC)
  , (s=="magmaFloatComplex", ComplexC FloatC)
  , (s=="magmaDoubleComplex", ComplexC DoubleC)
  , (True, EnumC s) ]
  where
  s = identToString ident
convert _ = VoidC

convert' :: [CDeclarationSpecifier a] -> TypeC
convert' (CTypeSpec x:_) = convert x
convert' (_:xs) = convert' xs
convert' [] = error "convert': invalid CDeclarationSpecifier list"

typeOf :: (TypeC -> Q Type) -> CDeclaration a -> Q Type
typeOf proj (CDecl basetype [(Just (CDeclr (Just ident) ptrs _ _ _), _, _)] _) = 
  foldr f (proj $ convert' basetype) ptrs
  where
  f (CPtrDeclr _ _) b = [t| F.Ptr $(b) |]
  f _ _ = error "haven't implemented other things"

pointerification :: CDeclaration a -> (TypeC -> TypeC)
pointerification (CDecl _ [(Just (CDeclr _ ptrs _ _ _), _, _)] _) = foldr (.) id $ map f ptrs where
  f (CPtrDeclr _ _) = PtrC
  f (CArrDeclr _ _ _) = ArrC
  f _ = id --possible there are other things that should be here?

baseType :: CDeclaration a -> TypeC
baseType (CDecl basetype _ _) = convert' basetype

cType :: CDeclaration a -> TypeC
cType d = (pointerification d) (baseType d)
  

typeInfo :: String -> CVar -> TypeInfo
typeInfo fn (n, typec) = ($ typec) $ try
  [ (case typec of ArrC _ -> True; _ -> False
      , inT)
  , (n `elem` []
      , inT)
  , (n == "info"
      , outT)
  , (n == "ipiv"
      , const (case typec of PtrC t -> useT (HPtrC t)))
    {- End -}
  , (True
      , useT)
  ]

declName :: CDeclaration a -> Maybe String
declName (CDecl _ [(Just (CDeclr (Just ident) _ _ _ _), _, _)] _) = Just (identToString ident)
declName _ = Nothing

outMarshall :: TypeC -> (Q Exp, Q Type -> Q Type)
outMarshall VoidC = ([| return . snd |], id)
outMarshall x = let hstype = hst $ typeDat x in
  ([| return |], \typ -> [t| ( $(hstype), $(typ) ) |])



createf' :: (String, CFunction) -> Q [Dec]
createf' (foreignname, cf@(fname, rettype, args)) = do
  ins <- mapM (safeName "_in") args
  toCs <- mapM (safeName "_out") args
  (outstatements, (outtypes, outs)) <- second (unzip . filterMaybes) . unzip <$> collect (zip3 args argsTI toCs)
  let instatements = map inMarsh (zip3 argsTI ins toCs)
  ret <- newName "res"
  let runstatement = bindS (varP ret) (foldl f z toCs)
  let returnstatement = [| $(checkStatusExp) ( $(outputConv) $(varE ret), $(tupE (map varE outs)) ) |]
  expr <- doE $ concat [instatements, runstatement:outstatements, [noBindS returnstatement]]
  let usedins = map snd . filter (isused . fst) $ zip argsTI ins
  let fdec = FunD fcall [Clause (map VarP usedins) (NormalB expr) []]
  tdec <- sigD fcall $ funTypeMod checkStatusType argsTI
  return [tdec, fdec]
  where
  safeName :: String -> CVar -> Q TH.Name
  safeName end (s:str, _) = newName (toLower s : str ++ end)

  argsTI = functionTypeInfo cf
  (outputConv, _) = c2hs (typeDat rettype)
  (checkStatusExp, checkStatusType) = outMarshall rettype
  isused (TI _ (Left _) _) = True
  isused _ = False
  fcall = mkName fname
  z = varE (mkName foreignname)
  f x e = appE x (varE e)
  inMarsh (ti,e,e') = case hsinput ti of
    Left (Convert t a) -> bindS (varP e') (appE a (varE e))
    Right (Create a)   -> bindS (varP e') a
  collect ( (arg, TI _ _ (Just cleanup), e) : xs) = do
    e' <- safeName "_out" arg
    let outinfo = case cleanup of
          Left (Convert t a) -> (bindS (varP e') (appE a (varE e)), Just (t, e'))
          Right (Destroy a) ->  (noBindS (appE a (varE e)), Nothing)
    ys <- collect xs
    return (outinfo:ys)
  collect (_:xs) = collect xs
  collect [] = return []
  collecti (TI _ (Left (Convert t _)) _) = 
    [t]
  collecti _ = []


magmaFile :: FilePath
magmaFile = MAGMA_INCLUDE_DIR </> "magma.h"


filterMaybes :: [Maybe a] -> [a]
filterMaybes [] = []
filterMaybes (Just x:xs) = x : filterMaybes xs
filterMaybes (Nothing:xs) = filterMaybes xs


funname :: CDeclaration a -> String
funname (CDecl _ [(Just (CDeclr (Just ident ) _ _ _ _), _, _)] _) = identToString ident
funname _ = error "Weird!"

infol :: Show a =>  CDerivedDeclarator a -> Maybe [[String]]
infol (CFunDeclr (Right (ys,_)) _ _) = Just $ map f ys where
  f (CDecl specs _ _) = map show specs
infol _ = Nothing

funArgs :: CDeclarator a -> Maybe [CDeclaration a]
funArgs (CDeclr _ [(CFunDeclr (Right (ys,_)) _ _)] _ _ _) = Just ys
funArgs _ = Nothing

funDecl :: CDeclaration a -> Maybe (CDeclarator a)
funDecl (CDecl _ [(Just declarator, _, _)] _) = Just declarator
funDecl _ = Nothing

maybeFunction :: CDeclaration a -> Maybe (CFunction)
maybeFunction d@(CDecl returnType _ _) = do
  args <- funArgs =<< funDecl d
  retName <- declName d
  argNames <- mapM declName args
  let argTypes = map cType args
  return (retName, convert' returnType, zip argNames argTypes )

maybeExternalDec :: CExternalDeclaration a -> Maybe (CDeclaration a)
maybeExternalDec (CDeclExt d) = Just d
maybeExternalDec _ = Nothing

type CVar = (String, TypeC)
type CFunction = ( String , TypeC , [CVar] )

getFunctions :: FilePath -> IO [CFunction]
getFunctions fp = do
  Right (CTranslUnit xs _) <- parseCFile (newGCC "/usr/bin/gcc") Nothing ["-DHAVE_CUBLAS", "-I" ++ CUDA_INCLUDE_DIR] fp
  return $ mapMaybe (maybeExternalDec >=> maybeFunction) xs

createf :: FilePath -> CFunction -> Q Dec
createf fp (name, ret, args) = 
  forImpD cCall safe{-unsafe-} (fp ++ ' ':name) (mkName name) cFunType
  where
  cFunType = foldr f z (map (ct . typeDat . snd) args)
  z = [t| IO $(ct . typeDat $ ret) |]
  f x y = [t| $(x) -> $(y) |]


parse' :: P.Parsec String () a -> String -> Maybe a
parse' p = \str -> case P.parse p "Unknown error" str of
  Right x -> Just x
  Left e -> Nothing

-- ["Magma", [("potrf_gpu", [("spotrf_gpu", spotrf_gpu )])]
sharedDecs :: String -> [CFunction] -> [(String, [Char], [(String, [(String, CFunction)])])]
sharedDecs prefix@(p:ps) xs = [(prefix' ++ "1", "sd", floatDouble), (prefix', "sdcz", all4)] where
  prefix' = toUpper p : ps
  g x@(s,ret,args) = do
    newname <- goodName prefix s
    return (s, (newname, ret, args))
  xs' = mapMaybe g xs
  fst3 (s,_,_) = s
  (floatDouble, all4) = partitionEithers . 
    mapMaybe sdFilter . groupWith (tail . fst3 . snd) $ xs'
  sdFilter xs@((_, (newname, _, _)) : _)
    | length xs == 4 = Just $ Right (newname, xs)
    | length xs == 2 = Just $ Left (newname, xs)
    | otherwise      = Nothing

mkClass :: String -> [CFunction] -> Q Dec
mkClass classname xs = classD (return []) className [PlainTV typeName] [] decs where
  className = mkName classname
  typeName = mkName "a"
  decs = map (f . phonifyF) xs
  mkPhony :: TypeC -> TypeC
  mkPhony (PtrC t) = PtrC (mkPhony t)
  mkPhony (ArrC t) = ArrC (mkPhony t)
  mkPhony x = let t' = PhonyC typeName in
    case x of DoubleC -> t'; FloatC -> t'; ComplexC _ -> t'; y -> y
  phonifyF :: CFunction -> CFunction
  phonifyF cf@(name, ret, args) = (name, mkPhony ret, map (second mkPhony) args)
  f cfunc@(name, ret, _) = 
     let (_, retTytrans) = outMarshall ret
         funtype = funTypeMod retTytrans (functionTypeInfo cfunc) 
     in sigD (mkName (tail name)) funtype

mkClassInstances :: String -> [Char] -> [(String, [(String,CFunction)])] -> [Q Dec]
mkClassInstances classname instances xs = map (\c -> makeInstance c $ map (f c) xs) instances where
  makeInstance c decs = instanceD (return []) classSig (decs) where
    classSig = appT (return . ConT $ mkName classname) (ct . typeDat $ typeMap c)
  f c (_, funcs) = (!! 1) <$> createf' (foreignn, (name, ret, args)) where
    [(foreignn,((_:name), ret, args))] = filter (\(_,((s:_),_,_))-> s==c) funcs

typeMap :: Char -> TypeC
typeMap 'c' = ComplexC FloatC
typeMap 'z' = ComplexC DoubleC
typeMap 'd' = DoubleC
typeMap 's' = FloatC
typeMap _ = error "typeMap: Invalid character"

makeClassDecs :: String -> FilePath -> IO (Q [Dec])
makeClassDecs str fp = do
  sds <- sharedDecs str <$> getFunctions fp
  return . sequence $ concat
   [ mkClass classname (map (snd . head . snd) funcs) : mkClassInstances classname instances funcs 
     | (classname, instances, funcs) <- sds ]

makeFFIDecs :: String -> FilePath -> IO (Q [Dec])
makeFFIDecs str fp = sequence . map (createf fp) . filter (desired str) <$> getFunctions fp

makeAllFuncs :: String -> FilePath -> IO (Q [Dec])
makeAllFuncs str fp = fmap concat . sequence . mapMaybe (fmap createf' . alter) . filter (desired str) <$> getFunctions fp where
  alter (fname, rettype, args) = do
    newname <- goodName str fname
    return (fname, (newname, rettype, args))

goodName :: String -> String -> Maybe String
goodName prefix = parse' $ do
    P.string prefix
    P.char '_'
    char <- P.oneOf "sdcz"
    res <- P.choice $ map (P.try . P.string) 
      ["gelqf", "gels3", {-"geqp3",-} "geqrf", "geqrs", "gesv", "getrf", "getrs", "potrf"
      , "potrs", "sposv", "syevd" ]
    P.string "_gpu"
    return (char : res)

desired :: String -> CFunction -> Bool
desired prefix = \(name, _, _) -> isJust $ goodName prefix name

doIO :: IO (Q [a]) -> Q [a]
doIO = join . runIO

inTypes :: [TypeInfo] -> [Q Type]
inTypes = mapMaybe f where
  f (TI _ (Left (Convert t _)) _) = Just t
  f _ = Nothing

outTypes :: [TypeInfo] -> [Q Type]
outTypes = mapMaybe f where
  f (TI _ _ (Just (Left (Convert t _)))) = Just t
  f _ = Nothing

functionTypeInfo :: CFunction -> [TypeInfo]
functionTypeInfo (fname, ret, args) = map (typeInfo fname) args

funTypeMod :: (Q Type -> Q Type) -> [TypeInfo] -> Q Type
funTypeMod f args = foldr arrow z ins where
  arrow x y = [t| $(x) -> $(y) |]
  z = [t| IO $( f $ foldl appT (tupleT (length outs)) outs) |]
  [ins, outs] = map ($ args) [inTypes, outTypes]

funType :: [TypeInfo] -> Q Type
funType = funTypeMod id
