package tree

sealed class Tree[+I] {
  def map[O](f: I => O): Tree[O] = this match {
    case TreeLeaf => TreeLeaf
    case TreeNode(d, ch) => TreeNode(f(d), ch.map(_.map(f)))
  }
  def flatMap[O](f: (I, Seq[Tree[I]]) => Tree[O]): Tree[O] = this match {
    case TreeLeaf => TreeLeaf
    case TreeNode(d, ch) => f(d, ch)
  }
  def foreach[T >: I](f: Tree[T] => Unit)(implicit traverseStrategy: TreeTraverseStrategy): Unit = {
    traverseStrategy.traverse(this, f)
  }
}
object Tree {
  def apply[T](data: T, children: Tree[T]*) = TreeNode(data, children)
}

case object TreeLeaf extends Tree[Nothing]
case class TreeNode[+T](data: T, children: Seq[Tree[T]]) extends Tree[T]

trait TreeTraverseStrategy[T] {
  def traverse[T](tree: Tree[T], processor: Tree[T] => Unit)
}

object DFSTreeTraverseStrategy extends TreeTraverseStrategy {
  override def traverse[T](tree: Tree[T], processor: Tree[T] => Unit): Unit = {
    tree match {
      case n @ TreeNode(d, ch) =>
        ch.map(n => traverse(n, processor))
        processor(n)
        ch.map(n => traverse(n, processor))
      case TreeLeaf =>
    }
  }
}

object BFSTreeTraverseStrategy extends TreeTraverseStrategy {
  override def traverse[T](tree: Tree[T], processor: Tree[T] => Unit): Unit = {

    def traverseLayer(layer: Seq[Tree[T]], processor: Tree[T] => Unit): Unit = {
      val nextLayer = layer.flatMap {
        case TreeNode(_, ch) => ch
        case TreeLeaf => List()
      }

      layer.foreach {
        case n: TreeNode[T] => processor(n)
        case TreeLeaf =>
      }

      if (nextLayer.nonEmpty)
        traverseLayer(nextLayer, processor)
    }

    traverseLayer(List(tree), processor)
  }
}