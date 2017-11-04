package com.thoughtworks.each

import scala.tools.nsc.{Global, Mode, Phase}
import scala.tools.nsc.plugins.{Plugin, PluginComponent}
import scala.tools.nsc.typechecker.ContextMode
private object CompilerPlugin {
  private[CompilerPlugin] object Reset
}
import CompilerPlugin._

/**
  * @author 杨博 (Yang Bo)
  */
final class CompilerPlugin(override val global: Global) extends Plugin {
  import global._
  import global.analyzer._

  private type EachAttachment = (Tree => Tree) => Tree

  val name: String = "each"

  val components: List[PluginComponent] = Nil

  val description: String =
    "A compiler plugin that converts native imperative syntax to monadic expressions or continuation-passing style expressions"

  val analyzerPlugin: AnalyzerPlugin = new AnalyzerPlugin {
    private val resetSymbol = symbolOf[EachOps.reset]

    private val EachSymbol = typeOf[EachOps[Any]].declaration(TermName("each"))
    private val BangSymbol = typeOf[EachOps[Any]].declaration(TermName("unary_!").encodedName)

    override def canAdaptAnnotations(tree: Tree, typer: Typer, mode: Mode, pt: Type): Boolean = {
      tree.tpe.annotations.exists { annotation =>
        annotation.matches(resetSymbol)
      }
    }

    override def adaptAnnotations(tree: Tree, typer: Typer, mode: Mode, pt: Type): Tree = {
      val Some(attachment) = tree.attachments.get[EachAttachment]
      val Seq(typedCpsTree) = tree.tpe.annotations.collect {
        case annotation if annotation.matches(resetSymbol) =>
          val cpsTree = resetAttrs(attachment(identity))
//          reporter.info(tree.pos, s"Translating to continuation-passing style: $cpsTree", false)
          deact {
            typer.context.withMode(ContextMode.ReTyping) {
              typer.typed(cpsTree, Mode.EXPRmode)
            }
          }
      }
      typedCpsTree
    }

    override def pluginsPt(pt: Type, typer: Typer, tree: Tree, mode: Mode): Type = {
      if (mode.inExprMode) {
        tree match {
          case function: Function =>
            function.body.updateAttachment(Reset)
          case defDef: DefDef =>
            defDef.rhs.updateAttachment(Reset)
          case _ =>
        }
      }
      pt
    }

    private def cpsAttachment(tree: Tree)(continue: Tree => Tree): Tree = {
      tree.attachments.get[EachAttachment] match {
        case Some(attachment) => attachment(continue)
        case None             => continue(tree)
      }
    }
    private def cpsParameter(parameters: List[Tree])(continue: List[Tree] => Tree): Tree = {
      parameters match {
        case Nil =>
          continue(Nil)
        case head :: tail =>
          cpsAttachment(head) { headValue =>
            cpsParameter(tail) { tailValues =>
              continue(headValue :: tailValues)
            }
          }
      }
    }
    private def cpsParameterList(parameterLists: List[List[Tree]])(continue: List[List[Tree]] => Tree): Tree = {
      parameterLists match {
        case Nil =>
          continue(Nil)
        case headList :: tailList =>
          cpsParameter(headList) { headValue =>
            cpsParameterList(tailList) { tailValues =>
              continue(headValue :: tailValues)
            }
          }
      }
    }

    private def isCpsTree(tree: Tree) = {
      def hasEachAttachment(child: Any): Boolean = {
        child match {
          case list: List[_]   => list.exists(hasEachAttachment)
          case childTree: Tree => childTree.hasAttachment[EachAttachment]
          case _               => false
        }
      }
      tree.productIterator.exists(hasEachAttachment)
    }

    private def isEachMethod(tree: Tree): Boolean = {
      val symbol = tree.symbol
      symbol match {
        case EachSymbol | BangSymbol => true
        case _                       => false
      }
    }
    override def pluginsTyped(tpe: Type, typer: Typer, tree: Tree, mode: Mode, pt: Type): Type = {
      def cps(continue: Tree => Tree): Tree = atPos(tree.pos) {
        tree match {
          case q"$prefix.$method[..$typeParameters](...$parameterLists)" =>
            cpsAttachment(prefix) { prefixValue =>
              cpsParameterList(parameterLists) { parameterListsValues =>
                atPos(tree.pos) {
                  q"$prefixValue.$method[..$typeParameters](...$parameterListsValues)"
                }
              }
            }
          // TODO: lazy val
          case ValDef(mods, name, tpt, rhs) =>
            cpsAttachment(rhs) { rhsValue =>
              atPos(tree.pos) {
                q"""
                ${treeCopy.ValDef(tree, mods, name, tpt, rhsValue)}
                ${continue(q"()")}
                """
              }
            }
          case Block(stats, expr) =>
            def loop(stats: List[Tree]): Tree = {
              stats match {
                case Nil =>
                  cpsAttachment(expr)(continue)
                case head :: tail =>
                  def notPure(head: Tree): List[Tree] = {
                    if (head.isInstanceOf[Ident]) {
                      Nil
                    } else {
                      head :: Nil
                    }
                  }
                  cpsAttachment(head) { headValue =>
                    q"..${notPure(headValue)}; ${loop(tail)}"
                  }
              }
            }
            loop(stats)
          case If(cond, thenp, elsep) =>
            val endIfName = currentUnit.freshTermName("endIf")
            val ifResultName = currentUnit.freshTermName("ifResult")
            val endIfBody = continue(q"$ifResultName")
            q"""
            @_root_.scala.inline def $endIfName($ifResultName: $tpe) = $endIfBody
            ${cpsAttachment(cond) { condValue =>
              atPos(tree.pos) {
                q"""
                if ($condValue) ${cpsAttachment(thenp) { result =>
                  q"$endIfName($result)"
                }} else ${cpsAttachment(elsep) { result =>
                  q"$endIfName($result)"
                }}
                """
              }
            }}
            """
          case _: CaseDef =>
            // This CaseDef tree contains some bang notations, and will be translated by enclosing Try tree, not here
            EmptyTree
          case Try(block, catches, finalizer) =>
            val finalizerName = currentUnit.freshTermName("finalizer")
            val tryResultName = currentUnit.freshTermName("tryResult")
            q"""
            @_root_.scala.inline def $finalizerName($tryResultName: $tpe) = ${cpsAttachment(finalizer) {
              finalizerValue =>
                q"""
                $finalizerValue
                ${continue(q"$tryResultName")}
              """
            }}
            ${cpsAttachment(block) { blockValue =>
              q"$finalizerName($blockValue)"
            }}.partialCatch {
              case ..${catches.map {
              case caseDef @ CaseDef(pat, guard, body) =>
                atPos(caseDef.pos) {
                  CaseDef(pat, guard, cpsAttachment(body) { bodyValue =>
                    q"$finalizerName($bodyValue)"
                  })
                }
            }}
            }
            """
        }
      }
      def checkResetAnnotation: Type = {
        tree.attachments.get[Reset.type] match {
          case None =>
            tpe
          case Some(_) =>
            tpe.withAnnotations(List(Annotation(deact {
              typer.context.withMode(ContextMode.NOmode) {
                typer.typed(q"new _root_.com.thoughtworks.each.EachOps.reset()", Mode.EXPRmode)
              }
            })))
        }
      }
      if (mode.inExprMode) {
        if (isEachMethod(tree)) {
          val q"$eachOps.$eachMethod" = tree
          val attachment: EachAttachment = { continue: (Tree => Tree) =>
            val aName = currentUnit.freshTermName("a")
            atPos(tree.pos) {
              q"""
                $eachOps { $aName: $tpe =>
                  ${continue(q"$aName")}
                }
              """
            }
          }
          tree.updateAttachment[EachAttachment](attachment)
          checkResetAnnotation
        } else if (isCpsTree(tree)) {
          tree.updateAttachment[EachAttachment](cps)
          checkResetAnnotation
        } else {
          tpe
        }

      } else {
        tpe
      }
    }

    private var active = true
    private def deact[A](run: => A): A = {
      synchronized {
        active = false
        try {
          run
        } finally {
          active = true
        }
      }
    }

    override def isActive(): Boolean = {
      active && phase.id < currentRun.picklerPhase.id
    }

  }

  global.analyzer.addAnalyzerPlugin(analyzerPlugin)

}
