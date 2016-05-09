import scala.annotation.tailrec

object AhoCorasick {

  private class Node(val value: Char, //symbol in pattern
                     val pattern: Option[String], //nonEmpty for last node in pattern, contains pattern As Is
                     val parent: Option[Node] = None //parent node
                    )

  @tailrec //represents pattern as list of nodes (parent-child relation), each node is a symbol in pattern
  private def patternHierarchyBuilder(patternAsStr: String,
                                      lastPatternIdx: Int,
                                      i: Int = 0,
                                      rootNode: Option[Node] = None,
                                      descendantsNodes: Map[Node, Node] = Map.empty, /*parent -> child*/
                                      parentNode: Option[Node] = None
                                     ): (Char, Node, Map[Node, Node]) /*first symbol in pattern, root node, all root's descendants*/ = {
    if (i > lastPatternIdx) {
      (rootNode.get.value, rootNode.get, descendantsNodes)
    } else {
      val matchPattern = if (i == lastPatternIdx) Some(patternAsStr) else None
      val currentNode = new Node(value = patternAsStr(i), pattern = matchPattern, parent = parentNode)
      val (root, descendants) = parentNode match {
        case Some(p) => (rootNode, descendantsNodes + (p -> currentNode))
        case None => (Some(currentNode), descendantsNodes)
      }

      patternHierarchyBuilder(patternAsStr, lastPatternIdx, i + 1, root, descendants, Some(currentNode))
    }
  }

  @tailrec
  private def processPatterns(listOfPatterns: Set[String],
                              roots: Map[Char, Set[Node]] = Map.empty,
                              descendants: Map[Node, Node] = Map.empty): (Map[Char, Set[Node]], Map[Node, Node]) = listOfPatterns match {
    case h if h.nonEmpty =>
      val (firstChar, currRoot, currDescendants) = patternHierarchyBuilder(h.head, h.head.length - 1)
      val updRoots: Map[Char, Set[Node]] = roots + (firstChar -> (roots.getOrElse(firstChar, Set.empty[Node]) + currRoot))
      val updDescendants: Map[Node, Node] = descendants ++ currDescendants

      processPatterns(listOfPatterns.tail, updRoots, updDescendants)
    case _ =>
      (roots, descendants)
  }


  def find(patterns: Set[String], string: String): List[(String, Int)] = {

    val lastIdxInStr = string.length - 1
    val (roots, descendants) = processPatterns(patterns.map(_.toLowerCase))

    @tailrec //find patterns in string
    def findPatterns(i: Int = 0,
                     parentNodes: Set[Node] = Set.empty,
                     result: List[(String, Int)] = List.empty): List[(String, Int)] /*pattern, it's end position in string*/ = {
      if (i > lastIdxInStr) {
        result
      } else {
        val currentChar = string(i).toLower

        val matchedDescendants = parentNodes
          .map(descendants(_))
          .filter(_.value == currentChar)
        val matchedRoots = roots.filter(_._1 == currentChar).values.flatten.toSet
        val matchedNodes: Set[Node] = matchedDescendants ++ matchedRoots

        val foundPatterns: List[(String, Int)] = matchedNodes
          .filter(_.pattern.isDefined)
          .map { n => (n.pattern.get, i) }
          .toList

        findPatterns(i + 1, matchedNodes.filter(_.pattern.isEmpty), result ++ foundPatterns)
      }
    }

    findPatterns()
  }
}
