package com.thoughtworks.dsl.delimitedcontinuation

import com.thoughtworks.dsl.delimitedcontinuation.annotations.{reset, shift}

import scala.tools.nsc.ast.TreeDSL
import scala.tools.nsc.plugins.{Plugin, PluginComponent}
import scala.tools.nsc.transform.{Transform, TypingTransformers}
import scala.tools.nsc.typechecker.ContextMode
import scala.tools.nsc.{Global, Mode, Phase}

/**
  * @author 杨博 (Yang Bo)
  */
final class CompilerPlugin(override val global: Global) extends Plugin with TypingTransformers {
  val name: String = "delimitedcontinuation"
  val description: String =
    "A compiler plugin that converts native imperative syntax to monadic expressions or continuation-passing style expressions"
  import global._
  import global.analyzer._

  sealed trait CpsTree {
    def cpsApply(continue: (Tree, Typer, Type) => Tree): Tree
  }

  trait PlainTree extends CpsTree {
    def plainTree: Tree
    def typer: Typer
    def domainType: Type
    // TODO: Add typer parameter here?
    final def cpsApply(continue: (Tree, Typer, Type) => Tree): Tree = continue(plainTree, typer, domainType)
  }

//  def cpsTree(domain: TypTree, tree: Tree): CpsTree = new CpsTree {
//    def toTree: Tree = tree
//  }

//  private final class CpsTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {
//
////    override def transform(tree: Tree): Tree = {
////      tree match {
////        case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
////          val transformedRhs = cpsTree(tpt.asInstanceOf[TypTree], rhs).toTree
////          atOwner()
////          treeCopy.DefDef(tree, mods, name, tparams, vparamss, tpt, transformedRhs)
////        case _ =>
////          super.transform(tree)
////      }
////    }
//    override def transformValDefss(treess: List[List[global.ValDef]]): List[List[global.ValDef]] =
//      super.transformValDefss(treess)
//  }

  val components: List[PluginComponent] = new PluginComponent {

    val global: CompilerPlugin.this.global.type = CompilerPlugin.this.global
    val phaseName: String = CompilerPlugin.this.name
    val runsAfter: List[String] = List("patmat") // Maybe typer is better?
    def newPhase(prev: Phase): Phase = new StdPhase(prev) {
      def apply(unit: CompilationUnit): Unit = {
        val rootTyper = analyzer.newTyper(analyzer.rootContextPostTyper(unit, EmptyTree))
        unit.body = cpsTransform(unit.body, rootTyper, definitions.UnitTpe).cpsApply {
          (rootTree, rootTyper, rootType) =>
            rootTree
        }
      }
    }
  } :: Nil

  private def cpsTransform(outerTree: Tree, outerTyper: Typer, expectedDomainType: Type): CpsTree = {
    // atOwner(tree.symbol):
    //   Function, ValDef, DefDef, ClassDef, TypeDef
    //   maybe LabelDef
    // atOwner(mclass(tree.symbol)):
    //   ModuleDef, PackageDef

    outerTree match {
      case PackageDef(pid, stats) =>
        new PlainTree {
          def typer = outerTyper
          def domainType = expectedDomainType
          def plainTree = {
            val subTyper = outerTyper.atOwner(outerTree, outerTree.symbol.moduleClass)
            treeCopy.PackageDef(outerTree, pid, stats.map { stat =>
              cpsTransform(stat, subTyper, definitions.UnitTpe).cpsApply { (statTree, _, _) =>
                statTree
              }
            })
          }
        }

      case ModuleDef(mods, name, impl @ Template(parents, self, body)) =>
        new PlainTree {
          def typer = outerTyper
          def domainType = expectedDomainType
          def plainTree: Tree = {
            val subTyper = outerTyper.atOwner(outerTree, outerTree.symbol.moduleClass)
            treeCopy.ModuleDef(
              outerTree,
              mods,
              name,
              treeCopy.Template(impl, parents, self, body.map { stat =>
                cpsTransform(stat, subTyper, definitions.UnitTpe).cpsApply { (statTree, _, _) =>
                  statTree
                }
              })
            )
          }
        }
      case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
        new PlainTree {
          def typer = outerTyper
          def domainType = expectedDomainType
          def plainTree: Tree = {
            val defTyper = outerTyper.atOwner(outerTree, outerTree.symbol)
            treeCopy.DefDef(outerTree,
                            mods,
                            name,
                            tparams,
                            vparamss,
                            tpt,
                            cpsTransform(rhs, defTyper, tpt.tpe).cpsApply { (rhsTree, _, _) =>
                              rhsTree

                            })
          }
        }
      case Block(stats, expr) =>
        val cpsStats = stats.map { stat =>
          cpsTransform(stat, typer, expectedDomainType) // FIXME: typer should be generated from foldLeft
        }
        val cpsExpr = cpsTransform(expr, typer, expectedDomainType)

        if (cpsExpr.isInstanceOf[PlainTree] && cpsStats.forall {
              _.isInstanceOf[PlainTree]
            }) {
          new PlainTree {
            def plainTree: Tree =
              treeCopy.Block(outerTree,
                             cpsStats.map { case statPlainTree: PlainTree => statPlainTree.plainTree },
                             cpsExpr.asInstanceOf[PlainTree].plainTree)

            def typer: Typer = outerTyper

            def domainType = expectedDomainType
          }
        } else {
          new CpsTree {
            def cpsApply(continue: (Tree, Typer, Type) => Tree): Tree = {
              def loop(cpsStats: List[CpsTree]): Tree = {
                cpsStats match {
                  case Nil =>
                    cpsExpr.cpsApply(continue)
                  case head :: tail =>
                    def notPure(head: Tree): List[Tree] = {
                      if (head.isInstanceOf[Ident]) {
                        Nil
                      } else {
                        head :: Nil
                      }
                    }

                    // TODO: domain type changing
                    head.cpsApply { (headValue, headTyper, expectedDomainType) =>
                      headTyper.typed(q"..${notPure(headValue)}; ${loop(tail)}", expectedDomainType)
                    }
                }
              }
              loop(cpsStats)
            }
          }
        }

      case q"$prefix.$methodName[..$typeParameters](...$parameterLists)" =>
        ???
//          new CpsTree {
//            def cpsApply(continue: (Tree, Typer) => Tree): Tree = {
//              def loop(cpsStats: List[CpsTree]): Tree = {
//                cpsStats match {
//                  case Nil =>
//                    cpsTransform(expr, typer).cpsApply { (rhsTree, statTyper) =>
//                      rhsTree
//
//                    }
//                  case head :: tail =>
//                    def notPure(head: Tree): List[Tree] = {
//                      if (head.isInstanceOf[Ident]) {
//                        Nil
//                      } else {
//                        head :: Nil
//                      }
//                    }
//
//                    // TODO: typer
//                    head.cpsApply { (headValue, headTyper) =>
//                      headTyper.typed(q"..${notPure(headValue)}; ${loop(tail)}")
//                    }
//                }
//              }
//              loop(cpsStats)
//            }
//          }
//        }
//      case _ =>
//        reporter.error(tree.pos, s"Unsupported tree: ${showRaw(tree)}")
//        new PlainTree {
//          def toTree: Tree = tree
//        }
    }

  }

}

object CompilerPlugin {
  trait CpsTransformer {
    val global: Global
    import global._
  }

//  final class CompilerPluginComponent(val global: Global) extends PluginComponent with Transform with TreeDSL {
//    val phaseName: String = "delimitedcontinuation"
//    val runsAfter: List[String] = "typer" :: Nil
//
//    protected def newTransformer(unit: CompilationUnit): Transformer = new TypingTransformer(unit) {
//      override def transform(tree: Tree): Tree = super.transform(tree)
//    }
//  }
//
}
