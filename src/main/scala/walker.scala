import org.jsoup._
import org.jsoup.nodes.Document

import scala.jdk.CollectionConverters._
import scala.util.Random


object walker {

def walk(start:String, steps:Int):Unit ={

    // Try to get data, may throw exception when fetching url.

    if(steps > 0) try {


      // Get the original html content from the url.
      val page: Document = Jsoup
        .connect(start)
        .get()


      // Remove CDATA tags so that we can parse the ratings data within them.
      val d: Document = Jsoup.parse(page.toString
        .replace("//<![CDATA[","")
        .replace("//]]>","")
        .replace("\n",""))


      // Getting related links that exist on the page.
      val newlinks: Seq[String] = d.select("a[href*=https://www.goodreads.com/book/show/]").asScala.toSeq.map(x => x.attr("href"))

      // Getting the ratings/review/to-read data .
      val alldata: List[String] = Jsoup.parse(d.getElementsByClass("reviewControls__ratingDetails reviewControls--left rating_graph") // Get div element that contains all the ratings data.
        .select("script").get(1).html()) //Get data from within the script tag.
        .select("span").get(1) // Get the second span element, the rest are redundant.
        .toString.split("\\D+").filter(_.nonEmpty).toList // Get only the numeric values within the string.

      // Get the data into a clean numeric form for processing.
      // If else needed to adjust for digits, i,e 4.2 versus 4.26.
      val cleaned: List[Any] = if(alldata(1).length==1){alldata.head.concat(alldata.tail.head).toFloat /10 :: alldata.tail.tail.map(x=> x.toInt)} else {alldata.head.concat(alldata.tail.head).toFloat /100 :: alldata.tail.tail.map(x=> x.toInt)}

      //Title.
      val title = d.title().split(" by ")

      val result = List[Any](title(0),title(1),cleaned(0),cleaned(1),cleaned(2),cleaned(3))
      //Print result.
      println(result)

      // Optional, take a break.
      Thread.sleep(1000)

      // Call again.
      walk(Random.shuffle(newlinks).head,steps-1)

    }
      catch{
        case e: Exception => println(s"AN EXCEPTION HAS OCCURED OF TYPE ${e}")
      }


    else println("done!")

  }

  walk("https://www.goodreads.com/book/show/19063.The_Book_Thief",100)

}
