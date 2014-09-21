import org.scalatest._
import com.topic.models.tokenizer._

class TokenizerSpec extends FlatSpec with Matchers{

  "The tokenizer" should "tokenize sentences correctly" in{

    val testString="My dog went to the park. It sniffed some flowers(which were green)!"

    val correctTokenization=List("my","dog","went","to","the","park",".","it","sniffed","some","flowers","-lrb-","which","were","green","-rrb-","!")

    val resultTokenization=StanfordTokenizer.tokenizeString(testString)

    correctTokenization should equal (resultTokenization)

  }
}
