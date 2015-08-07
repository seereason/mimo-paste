{-# LANGUAGE CPP, FlexibleContexts, ScopedTypeVariables, TemplateHaskell #-}

import App (theAppInfo)
import Control.Applicative ((<$>), pure)
import Control.Lens (view)
import Control.Monad.Reader
import Control.Monad.State (evalStateT)
import Control.Monad.Writer (MonadWriter, execWriterT, tell)
import Data.Default (def)
import Data.Map as Map (keys)
import Data.Monoid (mempty)
import Data.Set as Set (Set, empty, fromList, toList, difference)
import qualified Data.Set.Extra as Set
import Language.Haskell.TH as TH
import Language.Haskell.TH.Context (InstMap, tellUndeclared)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Path.Graph (pathGraphEdges)
import Language.Haskell.TH.Syntax as TH
import Language.Haskell.TH.TypeGraph.Arity (typeArity)
import Language.Haskell.TH.TypeGraph.Expand (E(E), expandType)
import Language.Haskell.TH.TypeGraph.Edges (simpleEdges)
import Language.Haskell.TH.TypeGraph.Prelude (constructorName, unlifted)
import Language.Haskell.TH.TypeGraph.Shape (constructorFieldTypes)
import Language.Haskell.TH.TypeGraph.Stack (StackElement)
import Language.Haskell.TH.TypeGraph.TypeGraph (edges, makeTypeGraph, VertexStatus)
import Language.Haskell.TH.TypeGraph.TypeInfo (makeTypeInfo)
import Language.Haskell.TH.TypeGraph.Vertex (etype, TGV, vsimple)
import MIMO.App (runAppGraph)
import MIMO.State (S(S))
import GHC.Prim
import Test.HUnit

main :: IO ()
main = runTestTT tests >>= putStrLn . show

-- | Given two sets @expected@ and @actual@, @unexpected@ contains
-- @(difference actual expected)@ and @missing@ contains
-- @(difference expected actual)@.
data SetDifferences a = SetDifferences {unexpected :: Set a, missing :: Set a} deriving (Eq, Ord, Show)

noDifferences :: forall a. SetDifferences a
noDifferences = SetDifferences {unexpected = empty, missing = empty}

setDifferences :: Ord a => Set a -> Set a -> SetDifferences a
setDifferences expected actual = SetDifferences {unexpected = Set.difference actual expected, missing = Set.difference expected actual}

tests :: Test
tests =
    TestList
    [ TestCase (assertEqual "reify 3"
#if MIN_VERSION_template_haskell(2,10,0)
                  ("DataD [] Language.Haskell.TH.Syntax.NameFlavour [] [" ++
                    "NormalC Language.Haskell.TH.Syntax.NameS []," ++
                    "NormalC Language.Haskell.TH.Syntax.NameQ [(NotStrict,ConT Language.Haskell.TH.Syntax.ModName)]," ++
                    "NormalC Language.Haskell.TH.Syntax.NameU [(Unpacked,ConT GHC.Types.Int)]," ++
                    "NormalC Language.Haskell.TH.Syntax.NameL [(Unpacked,ConT GHC.Types.Int)]," ++
                    "NormalC Language.Haskell.TH.Syntax.NameG [" ++
                     "(NotStrict,ConT Language.Haskell.TH.Syntax.NameSpace)," ++
                     "(NotStrict,ConT Language.Haskell.TH.Syntax.PkgName)," ++
                     "(NotStrict,ConT Language.Haskell.TH.Syntax.ModName)]] []")
#else
                  ("DataD [] Language.Haskell.TH.Syntax.NameFlavour [] [" ++
                    "NormalC Language.Haskell.TH.Syntax.NameS []," ++
                    "NormalC Language.Haskell.TH.Syntax.NameQ [(NotStrict,ConT Language.Haskell.TH.Syntax.ModName)]," ++
                    "NormalC Language.Haskell.TH.Syntax.NameU [(NotStrict,ConT GHC.Prim.Int#)]," ++
                    "NormalC Language.Haskell.TH.Syntax.NameL [(NotStrict,ConT GHC.Prim.Int#)]," ++
                    "NormalC Language.Haskell.TH.Syntax.NameG [" ++
                     "(NotStrict,ConT Language.Haskell.TH.Syntax.NameSpace)," ++
                     "(NotStrict,ConT Language.Haskell.TH.Syntax.PkgName)," ++
                     "(NotStrict,ConT Language.Haskell.TH.Syntax.ModName)]] []")
#endif
                  $(let doInfo (TyConI dec) = return (show dec)
                        doInfo (PrimTyConI name num flag) = return (show (name, num, flag))
                        doInfo info = error $ "Unexpected: " ++ show info in
                    runReaderT (qLookupName True "NameFlavour" >>= \ (Just name) ->
                                qReify name >>= doInfo >>= runQ . TH.lift) ([] :: [StackElement])))

    , TestCase (assertEqual "reify 4"
                  "(GHC.Prim.Int#,0,True)"
                  $(let doInfo (TyConI dec) = return (show dec)
                        doInfo (PrimTyConI name num flag) = return (show (name, num, flag))
                        doInfo info = error $ "Unexpected: " ++ show info in
                    runReaderT (qLookupName True "Int#" >>= \ (Just name) ->
                                qReify name >>= doInfo >>= runQ . TH.lift) ([] :: [StackElement])))

    , let s = fromList ["Char","OccName","ModName","NameSpace","PkgName","NameFlavour","Name","Integer","Word8","Lit","TyVarBndr","Int",
                        "TyLit","Type","Stmt","Guard","Body","Match", "Range", "Exp","Pat","Clause","Strict","Con","FunDep","Callconv",
                        "Safety","Foreign","FixityDirection","Fixity","Inline","RuleMatch","Phases","RuleBndr","AnnTarget","Pragma","FamFlavour","TySynEqn","Role","Dec",
#if MIN_VERSION_template_haskell(2,10,0)
                        "BigNat"
#else
                        "Cxt","FieldExp","FieldPat","Kind","Maybe","Pred","Ratio","Rational","StrictType","String","VarStrictType"
#endif
                       ] in
      TestCase (assertEqual "findSubtypes"
                noDifferences
                (setDifferences
                  s
                  (fromList
                   $(let guard :: MonadWriter [Dec] m => TGV -> m (VertexStatus Type)
                         guard = const $ return def
                         doType :: Quasi m => MonadWriter [Dec] m => E Type -> m ()
                         doType (E (ConT name)) = qReify name >>= doInfo
                         doType _ = return ()
                         doInfo :: forall m. (Quasi m, MonadWriter [Dec] m) => Info -> m ()
                         doInfo (TyConI dec) = tell [dec]
                         doInfo _ = return ()
                         someType = ConT ''Dec
                         decName :: Dec -> Name
                         decName (NewtypeD _ name _ _ _) = name
                         decName (DataD _ name _ _ _) = name
                         decName (TySynD name _ _) = name
                         decName x = error $ "decName - unimplemented: " ++ show x in
                     execWriterT $ flip evalStateT (S mempty mempty mempty) $
                       makeTypeInfo (const $ return mempty) [someType] >>=
                       runReaderT (pathGraphEdges >>= makeTypeGraph) >>= \r ->
                       runReaderT (mapM_ (doType . view (vsimple . etype)) (keys (view edges r))) r >>
                       tellUndeclared) >>= \ (decs :: [Dec]) ->
                     TH.lift . map (nameBase . decName) $ decs)))

    , let s = fromList [
#if MIN_VERSION_template_haskell(2,10,0)
                             "TGVSimple {_syns = fromList [GHC.Base.String], _etype = E (AppT ListT (ConT GHC.Types.Char))}",
                             "TGVSimple {_syns = fromList [Language.Haskell.TH.Syntax.Cxt], _etype = E (AppT ListT (ConT Language.Haskell.TH.Syntax.Type))}",
                             "TGVSimple {_syns = fromList [Language.Haskell.TH.Syntax.Kind,Language.Haskell.TH.Syntax.Pred], _etype = E (ConT Language.Haskell.TH.Syntax.Type)}",
                             "TGVSimple {_syns = fromList [], _etype = E (AppT ListT (ConT Language.Haskell.TH.Syntax.TyVarBndr))}",
                             "TGVSimple {_syns = fromList [], _etype = E (ConT GHC.Integer.Type.BigNat)}",
                             "TGVSimple {_syns = fromList [], _etype = E (ConT GHC.Integer.Type.Integer)}",
                             "TGVSimple {_syns = fromList [], _etype = E (ConT GHC.Types.Char)}",
                             "TGVSimple {_syns = fromList [], _etype = E (ConT GHC.Types.Int)}",
                             "TGVSimple {_syns = fromList [], _etype = E (ConT Language.Haskell.TH.Syntax.ModName)}",
                             "TGVSimple {_syns = fromList [], _etype = E (ConT Language.Haskell.TH.Syntax.Name)}",
                             "TGVSimple {_syns = fromList [], _etype = E (ConT Language.Haskell.TH.Syntax.NameFlavour)}",
                             "TGVSimple {_syns = fromList [], _etype = E (ConT Language.Haskell.TH.Syntax.NameSpace)}",
                             "TGVSimple {_syns = fromList [], _etype = E (ConT Language.Haskell.TH.Syntax.OccName)}",
                             "TGVSimple {_syns = fromList [], _etype = E (ConT Language.Haskell.TH.Syntax.PkgName)}",
                             "TGVSimple {_syns = fromList [], _etype = E (ConT Language.Haskell.TH.Syntax.TyLit)}",
                             "TGVSimple {_syns = fromList [], _etype = E (ConT Language.Haskell.TH.Syntax.TyVarBndr)}"
#else
                             "AppT ListT (ConT GHC.Types.Char)",
                             "AppT ListT (ConT Language.Haskell.TH.Syntax.Pred)",
                             "AppT ListT (ConT Language.Haskell.TH.Syntax.TyVarBndr)",
                             "AppT ListT (ConT Language.Haskell.TH.Syntax.Type)",
                             "ConT GHC.Base.String",
                             "ConT GHC.Prim.ByteArray#",
                             "ConT GHC.Prim.Char#",
                             "ConT GHC.Prim.Int#",
                             "ConT GHC.Types.Char",
                             "ConT GHC.Types.Int",
                             "ConT GHC.Integer.Type.Integer",
                             "ConT Language.Haskell.TH.Syntax.Cxt",
                             "ConT Language.Haskell.TH.Syntax.Kind",
                             "ConT Language.Haskell.TH.Syntax.ModName",
                             "ConT Language.Haskell.TH.Syntax.Name",
                             "ConT Language.Haskell.TH.Syntax.NameFlavour",
                             "ConT Language.Haskell.TH.Syntax.NameSpace",
                             "ConT Language.Haskell.TH.Syntax.OccName",
                             "ConT Language.Haskell.TH.Syntax.PkgName",
                             "ConT Language.Haskell.TH.Syntax.Pred",
                             "ConT Language.Haskell.TH.Syntax.TyLit",
                             "ConT Language.Haskell.TH.Syntax.TyVarBndr",
                             "ConT Language.Haskell.TH.Syntax.Type",
                             "ListT"
#endif
                             ] in
      TestCase (assertEqual "enumerateSubtypes of Type"
                noDifferences
                (setDifferences s
                  (fromList $(sequence [ [t|Type|] ] >>= makeTypeInfo (const $ return mempty) >>= runReaderT (pathGraphEdges >>= makeTypeGraph) >>= \r -> TH.lift (map show (keys (simpleEdges (view edges r))))))))
                  -- (fromList $([t|Type|] >>= flip evalStateT (mempty :: InstMap (E Pred)) . typeGraphVertices (const $ return def) . (: []) >>= TH.lift . map show . toList))))

    , let s = (fromList [
                             "(Guard, Exp)",
                             "(Name, Exp) (aka FieldExp)",
                             "(Name, Pat) (aka FieldPat)",
                             "(Name, Strict, Type) (aka VarStrictType)",
                             "(Strict, Type) (aka StrictType)",
                             "AnnTarget",
                             "BigNat",
                             "Body",
                             "Callconv",
                             "Char",
                             "Clause",
                             "Con",
                             "Dec",
                             "Exp",
                             "FamFlavour",
                             "Fixity",
                             "FixityDirection",
                             "Foreign",
                             "FunDep",
                             "Guard",
                             "Inline",
                             "Int",
                             "Integer",
                             "Lit",
                             "Match",
                             "Maybe Exp",
                             "Maybe Inline",
                             "Maybe Type",
                             "ModName",
                             "Name",
                             "NameFlavour",
                             "NameSpace",
                             "OccName",
                             "Pat",
                             "Phases",
                             "PkgName",
                             "Pragma",
                             "Range",
                             "Ratio Integer (aka Rational)",
                             "Role",
                             "RuleBndr",
                             "RuleMatch",
                             "Safety",
                             "Stmt",
                             "Strict",
                             "TyLit",
                             "TySynEqn",
                             "TyVarBndr",
                             "Type (aka Kind, aka Pred)",
                             "Word8",
                             "[(Guard, Exp)]",
                             "[(Name, Exp)]",
                             "[(Name, Pat)]",
                             "[(Name, Strict, Type)]",
                             "[(Strict, Type)]",
                             "[Char] (aka String)",
                             "[Clause]",
                             "[Con]",
                             "[Dec]",
                             "[Exp]",
                             "[FunDep]",
                             "[Match]",
                             "[Name]",
                             "[Pat]",
                             "[Role]",
                             "[RuleBndr]",
                             "[Stmt]",
                             "[TySynEqn]",
                             "[TyVarBndr]",
                             "[Type] (aka Cxt)",
                             "[Word8]",
                             "[[Stmt]]"
                             ]) in
      TestCase (assertEqual "enumerateSubtypes of Dec"
                  noDifferences
                  (setDifferences s
                   (fromList $(sequence [ [t|Dec|] ] >>= makeTypeInfo (const $ return mempty) >>= runReaderT (pathGraphEdges >>= makeTypeGraph) >>= \r -> TH.lift (map (unwords . words . pprint) (keys (simpleEdges (view edges r))))))))

    , let s = (fromList [
                             "(Language.Haskell.TH.Syntax.Guard, Language.Haskell.TH.Syntax.Exp)",
                             "(Language.Haskell.TH.Syntax.Name, Language.Haskell.TH.Syntax.Exp)",
                             "(Language.Haskell.TH.Syntax.Name, Language.Haskell.TH.Syntax.Pat)",
                             "(Language.Haskell.TH.Syntax.Name, Language.Haskell.TH.Syntax.Strict, Language.Haskell.TH.Syntax.Type)",
                             "(Language.Haskell.TH.Syntax.Strict, Language.Haskell.TH.Syntax.Type)",
                             "GHC.Base.Maybe Language.Haskell.TH.Syntax.Exp",
                             "GHC.Base.Maybe Language.Haskell.TH.Syntax.Inline",
                             "GHC.Base.Maybe Language.Haskell.TH.Syntax.Type",
                             "GHC.Integer.Type.BigNat",
                             "GHC.Integer.Type.Integer",
                             "GHC.Real.Ratio GHC.Integer.Type.Integer",
                             "GHC.Types.Char",
                             "GHC.Types.Int",
                             "GHC.Word.Word8",
                             "Language.Haskell.TH.Syntax.AnnTarget",
                             "Language.Haskell.TH.Syntax.Body",
                             "Language.Haskell.TH.Syntax.Callconv",
                             "Language.Haskell.TH.Syntax.Clause",
                             "Language.Haskell.TH.Syntax.Con",
                             "Language.Haskell.TH.Syntax.Dec",
                             "Language.Haskell.TH.Syntax.Exp",
                             "Language.Haskell.TH.Syntax.FamFlavour",
                             "Language.Haskell.TH.Syntax.Fixity",
                             "Language.Haskell.TH.Syntax.FixityDirection",
                             "Language.Haskell.TH.Syntax.Foreign",
                             "Language.Haskell.TH.Syntax.FunDep",
                             "Language.Haskell.TH.Syntax.Guard",
                             "Language.Haskell.TH.Syntax.Inline",
                             "Language.Haskell.TH.Syntax.Lit",
                             "Language.Haskell.TH.Syntax.Match",
                             "Language.Haskell.TH.Syntax.ModName",
                             "Language.Haskell.TH.Syntax.Name",
                             "Language.Haskell.TH.Syntax.NameFlavour",
                             "Language.Haskell.TH.Syntax.NameSpace",
                             "Language.Haskell.TH.Syntax.OccName",
                             "Language.Haskell.TH.Syntax.Pat",
                             "Language.Haskell.TH.Syntax.Phases",
                             "Language.Haskell.TH.Syntax.PkgName",
                             "Language.Haskell.TH.Syntax.Pragma",
                             "Language.Haskell.TH.Syntax.Range",
                             "Language.Haskell.TH.Syntax.Role",
                             "Language.Haskell.TH.Syntax.RuleBndr",
                             "Language.Haskell.TH.Syntax.RuleMatch",
                             "Language.Haskell.TH.Syntax.Safety",
                             "Language.Haskell.TH.Syntax.Stmt",
                             "Language.Haskell.TH.Syntax.Strict",
                             "Language.Haskell.TH.Syntax.TyLit",
                             "Language.Haskell.TH.Syntax.TySynEqn",
                             "Language.Haskell.TH.Syntax.TyVarBndr",
                             "Language.Haskell.TH.Syntax.Type",
                             "[(Language.Haskell.TH.Syntax.Guard, Language.Haskell.TH.Syntax.Exp)]",
                             "[(Language.Haskell.TH.Syntax.Name, Language.Haskell.TH.Syntax.Exp)]",
                             "[(Language.Haskell.TH.Syntax.Name, Language.Haskell.TH.Syntax.Pat)]",
                             "[(Language.Haskell.TH.Syntax.Name, Language.Haskell.TH.Syntax.Strict, Language.Haskell.TH.Syntax.Type)]",
                             "[(Language.Haskell.TH.Syntax.Strict, Language.Haskell.TH.Syntax.Type)]",
                             "[GHC.Types.Char]",
                             "[GHC.Word.Word8]",
                             "[Language.Haskell.TH.Syntax.Clause]",
                             "[Language.Haskell.TH.Syntax.Con]",
                             "[Language.Haskell.TH.Syntax.Dec]",
                             "[Language.Haskell.TH.Syntax.Exp]",
                             "[Language.Haskell.TH.Syntax.FunDep]",
                             "[Language.Haskell.TH.Syntax.Match]",
                             "[Language.Haskell.TH.Syntax.Name]",
                             "[Language.Haskell.TH.Syntax.Pat]",
                             "[Language.Haskell.TH.Syntax.Role]",
                             "[Language.Haskell.TH.Syntax.RuleBndr]",
                             "[Language.Haskell.TH.Syntax.Stmt]",
                             "[Language.Haskell.TH.Syntax.TySynEqn]",
                             "[Language.Haskell.TH.Syntax.TyVarBndr]",
                             "[Language.Haskell.TH.Syntax.Type]",
                             "[[Language.Haskell.TH.Syntax.Stmt]]"
                        ]) in
      TestCase (assertEqual "arity 0 subtypes"
                noDifferences
                (setDifferences s
                  (fromList $(sequence [[t|Dec|]] >>= makeTypeInfo (const $ return mempty) >>= runReaderT (pathGraphEdges >>= makeTypeGraph) >>= \r ->
                              return (map (view etype) (keys (simpleEdges (view edges r)))) >>=
                              filterM (\(E typ) -> typeArity typ >>= return . (== 0)) >>=
                              TH.lift . map (unwords . words . pprint . (id :: E Type -> E Type))))))

    , let s = (fromList [
                             "(Language.Haskell.TH.Syntax.Guard, Language.Haskell.TH.Syntax.Exp)",
                             "(Language.Haskell.TH.Syntax.Name, Language.Haskell.TH.Syntax.Exp)",
                             "(Language.Haskell.TH.Syntax.Name, Language.Haskell.TH.Syntax.Pat)",
                             "(Language.Haskell.TH.Syntax.Name, Language.Haskell.TH.Syntax.Strict, Language.Haskell.TH.Syntax.Type)",
                             "(Language.Haskell.TH.Syntax.Strict, Language.Haskell.TH.Syntax.Type)",
                             "GHC.Base.Maybe Language.Haskell.TH.Syntax.Exp",
                             "GHC.Base.Maybe Language.Haskell.TH.Syntax.Inline",
                             "GHC.Base.Maybe Language.Haskell.TH.Syntax.Type",
                             "GHC.Integer.Type.BigNat",
                             "GHC.Integer.Type.Integer",
                             "GHC.Real.Ratio GHC.Integer.Type.Integer",
                             "GHC.Types.Char",
                             "GHC.Types.Int",
                             "GHC.Word.Word8",
                             "Language.Haskell.TH.Syntax.AnnTarget",
                             "Language.Haskell.TH.Syntax.Body",
                             "Language.Haskell.TH.Syntax.Callconv",
                             "Language.Haskell.TH.Syntax.Clause",
                             "Language.Haskell.TH.Syntax.Con",
                             "Language.Haskell.TH.Syntax.Dec",
                             "Language.Haskell.TH.Syntax.Exp",
                             "Language.Haskell.TH.Syntax.FamFlavour",
                             "Language.Haskell.TH.Syntax.Fixity",
                             "Language.Haskell.TH.Syntax.FixityDirection",
                             "Language.Haskell.TH.Syntax.Foreign",
                             "Language.Haskell.TH.Syntax.FunDep",
                             "Language.Haskell.TH.Syntax.Guard",
                             "Language.Haskell.TH.Syntax.Inline",
                             "Language.Haskell.TH.Syntax.Lit",
                             "Language.Haskell.TH.Syntax.Match",
                             "Language.Haskell.TH.Syntax.ModName",
                             "Language.Haskell.TH.Syntax.Name",
                             "Language.Haskell.TH.Syntax.NameFlavour",
                             "Language.Haskell.TH.Syntax.NameSpace",
                             "Language.Haskell.TH.Syntax.OccName",
                             "Language.Haskell.TH.Syntax.Pat",
                             "Language.Haskell.TH.Syntax.Phases",
                             "Language.Haskell.TH.Syntax.PkgName",
                             "Language.Haskell.TH.Syntax.Pragma",
                             "Language.Haskell.TH.Syntax.Range",
                             "Language.Haskell.TH.Syntax.Role",
                             "Language.Haskell.TH.Syntax.RuleBndr",
                             "Language.Haskell.TH.Syntax.RuleMatch",
                             "Language.Haskell.TH.Syntax.Safety",
                             "Language.Haskell.TH.Syntax.Stmt",
                             "Language.Haskell.TH.Syntax.Strict",
                             "Language.Haskell.TH.Syntax.TyLit",
                             "Language.Haskell.TH.Syntax.TySynEqn",
                             "Language.Haskell.TH.Syntax.TyVarBndr",
                             "Language.Haskell.TH.Syntax.Type",
                             "[(Language.Haskell.TH.Syntax.Guard, Language.Haskell.TH.Syntax.Exp)]",
                             "[(Language.Haskell.TH.Syntax.Name, Language.Haskell.TH.Syntax.Exp)]",
                             "[(Language.Haskell.TH.Syntax.Name, Language.Haskell.TH.Syntax.Pat)]",
                             "[(Language.Haskell.TH.Syntax.Name, Language.Haskell.TH.Syntax.Strict, Language.Haskell.TH.Syntax.Type)]",
                             "[(Language.Haskell.TH.Syntax.Strict, Language.Haskell.TH.Syntax.Type)]",
                             "[GHC.Types.Char]",
                             "[GHC.Word.Word8]",
                             "[Language.Haskell.TH.Syntax.Clause]",
                             "[Language.Haskell.TH.Syntax.Con]",
                             "[Language.Haskell.TH.Syntax.Dec]",
                             "[Language.Haskell.TH.Syntax.Exp]",
                             "[Language.Haskell.TH.Syntax.FunDep]",
                             "[Language.Haskell.TH.Syntax.Match]",
                             "[Language.Haskell.TH.Syntax.Name]",
                             "[Language.Haskell.TH.Syntax.Pat]",
                             "[Language.Haskell.TH.Syntax.Role]",
                             "[Language.Haskell.TH.Syntax.RuleBndr]",
                             "[Language.Haskell.TH.Syntax.Stmt]",
                             "[Language.Haskell.TH.Syntax.TySynEqn]",
                             "[Language.Haskell.TH.Syntax.TyVarBndr]",
                             "[Language.Haskell.TH.Syntax.Type]",
                             "[[Language.Haskell.TH.Syntax.Stmt]]"
                        ]) in
      TestCase (assertEqual "non-primitive arity 0 subtypes"
                  noDifferences
                  (setDifferences s
                   (fromList $(sequence [[t|Dec|]] >>= makeTypeInfo (const $ return mempty) >>= runReaderT (pathGraphEdges >>= makeTypeGraph) >>= \r ->
                               return (map (view etype) (keys (simpleEdges (view edges r)))) >>=
                               filterM (\ (E typ) -> typeArity typ >>= return . (== 0)) >>=
                               filterM (\ (E typ) -> unlifted typ >>= return . not) >>=
                               TH.lift . map (unwords . words . pprint . (id :: E Type -> E Type))))))
    ]
