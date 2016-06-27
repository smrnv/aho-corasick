import org.scalatest._

class AhoCorasickSpec extends WordSpec {

  "Aho-Corasik finder" should {
    "find 10 matches in file" in {
      val result = AhoCorasick.find(Set("ста", "Ста"), getFileAsString("article.txt"))
      assert(result.length == 10)
    }

    "find 14 matches in file" in {
      val result = AhoCorasick.find(Set("он", "она"), getFileAsString("article.txt"))
      assert(result.length == 14)
    }

    "find 327 matches in file" in {
      val result = AhoCorasick.find(Set("о"), getFileAsString("article.txt"))
      assert(result.length == 327)
    }

    "find 3 patterns in string in 4 positions" in {
      val result = AhoCorasick.find(Set("she", "he", "Her"), "Ohhsherfhe")
      assert(result == List(("she", 5), ("he", 5), ("her", 6), ("he", 9)))
    }

    "return empty result" in {
      val result = AhoCorasick.find(Set("she", "he", "her"), "")
      assert(result == List.empty)
    }

    "return empty result again" in {
      val result = AhoCorasick.find(Set.empty, "Ohhsherfhe")
      assert(result == List.empty)
    }
  }

  private def getFileAsString(fileName: String): String = {
    val filePath = getClass.getClassLoader.getResource(fileName).getPath
    scala.io.Source.fromFile(filePath).mkString
  }

}
