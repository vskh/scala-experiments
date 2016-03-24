import bintree.{BinaryTree, BinaryTreeLeaf, BinaryTreeNode}

/*

        1
      2   3
     4 5 6 7

*/

val tree = BinaryTree(
  BinaryTreeNode(
    BinaryTreeNode(
      BinaryTreeLeaf,
      BinaryTreeLeaf,
      4
    ),
    BinaryTreeNode(
      BinaryTreeLeaf,
      BinaryTreeLeaf,
      5
    ),
    2
  ),
  BinaryTreeNode(
    BinaryTreeNode(
      BinaryTreeLeaf,
      BinaryTreeLeaf,
      6
    ),
    BinaryTreeNode(
      BinaryTreeLeaf,
      BinaryTreeLeaf,
      7
    ),
    3
  ),
  1
)

implicit val traverseStrategy = bintree.DFSBinaryTreeTraverseStrategy
tree.foreach(n => println(n.data))

