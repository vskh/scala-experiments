package bintree

sealed abstract class BinaryTree[+I] {
  def map[O](f: I => O): BinaryTree[O] = this match {
    case BinaryTreeLeaf => BinaryTreeLeaf
    case BinaryTreeNode(l, r, d) => BinaryTreeNode(l.map(f), r.map(f), f(d))
  }

  def flatMap[O](f: (BinaryTree[I], BinaryTree[I], I) => BinaryTree[O]): BinaryTree[O] = this match {
    case BinaryTreeLeaf => BinaryTreeLeaf
    case BinaryTreeNode(l, r, d) => f(l, r, d)
  }

  def foreach[T >: I](f: BinaryTreeNode[T] => Unit)(implicit traverseStrategy: BinaryTreeTraverseStrategy): Unit = {
    traverseStrategy.traverse(this, f)
  }
}

case object BinaryTreeLeaf extends BinaryTree[Nothing]

case class BinaryTreeNode[+T](left: BinaryTree[T], right: BinaryTree[T], data: T) extends BinaryTree[T]

object BinaryTree {
  def apply[T](left: BinaryTree[T], right: BinaryTree[T], data: T) = new BinaryTreeNode[T](left, right, data)
}

abstract class BinaryTreeTraverseStrategy {
  def traverse[T](tree: BinaryTree[T], processor: BinaryTreeNode[T] => Unit)
}

object DFSBinaryTreeTraverseStrategy extends BinaryTreeTraverseStrategy {
  override def traverse[T](tree: BinaryTree[T], processor: BinaryTreeNode[T] => Unit): Unit = {
    tree match {
      case n @ BinaryTreeNode(l, r, d) =>
        traverse(l, processor)
        processor(n)
        traverse(r, processor)
      case BinaryTreeLeaf =>
    }
  }
}

object BFSBinaryTreeTraverseStrategy extends BinaryTreeTraverseStrategy {
  override def traverse[T](tree: BinaryTree[T], processor: (BinaryTreeNode[T]) => Unit): Unit = {

    def traverseLayer(layer: List[BinaryTree[T]], processor: BinaryTreeNode[T] => Unit): Unit = {
      val nextLayer = layer.flatMap {
        case BinaryTreeNode(l, r, _) => List(l, r)
        case BinaryTreeLeaf => List()
      }

      layer.foreach {
        case n: BinaryTreeNode[T] => processor(n)
        case BinaryTreeLeaf =>
      }

      if (nextLayer.nonEmpty)
        traverseLayer(nextLayer, processor)
    }

    traverseLayer(List(tree), processor)
  }
}
