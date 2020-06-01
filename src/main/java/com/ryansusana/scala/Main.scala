package com.ryansusana.scala

import java.util.stream.Collectors

import com.google.cloud.functions.{HttpFunction, HttpRequest, HttpResponse}
import com.google.cloud.language.v1beta2.Document.Type
import com.google.cloud.language.v1beta2.{Document, LanguageServiceClient, Sentence}
import com.itextpdf.text.pdf.PdfReader
import com.itextpdf.text.pdf.parser.PdfTextExtractor

import scala.io.Source
import scala.jdk.CollectionConverters._

class Main extends HttpFunction {
  val language: LanguageServiceClient = LanguageServiceClient.create

  def service(request: HttpRequest, response: HttpResponse): Unit = {

    // Request.getParts is a Map<String, Part>

    val contentType = request.getContentType.orElse("application/json")

    val writer = response.getWriter

    if (!(contentType contains "multipart") || request.getParts.isEmpty) {
      writer.write("No files provided")
    } else {

      // Perform sentiment analysis
      try {
        val details = request.getParts.asScala.values.map(partToDetail).mkString("\n---\n");
        writer.write(details);
      } catch {
        case e: Exception => {
          e.printStackTrace();
          writer.write(e.getMessage)
        };
      }


    }
  }

  def pdfToString() = {

    val reader = new PdfReader("c:/temp/test.pdf")

    val r1 = 0 until 10
    val r2 = r1.start until r1.end by r1.step + 1

    def r5 = (1 to 5).map { e:Int => None }




    val page = PdfTextExtractor.getTextFromPage(reader, 0)

  }

  def partToDetail(part: HttpRequest.HttpPart): String = {

    val source = Source.fromInputStream(part.getInputStream).mkString


    detailText(part.getFileName.orElse("unknown"), source)
  }


  def detailText(fileName: String, input: String): String = {
    val doc = Document.newBuilder.setContent(input).setType(Type.PLAIN_TEXT).build

    val sentiment = language.analyzeSentiment(doc)

    val mostImpactfulSentence = sentiment.getSentencesList
      .asScala
      .max { (s1: Sentence, s2: Sentence) => s1.getSentiment.getMagnitude compareTo s2.getSentiment.getMagnitude }
      .getText.getContent

    val mostHappySentence = sentiment.getSentencesList
      .asScala
      .max { (s1: Sentence, s2: Sentence) => s1.getSentiment.getScore compareTo s2.getSentiment.getScore }
      .getText.getContent

    val score = sentiment.getDocumentSentiment.getScore

    val toneOfText = if (score > 0.2) "positive" else if (score <= 0.2) "negative" else "neutral"


    val x = sentiment.getSentencesList.stream()
      .map { s => s"(Score: ${s.getSentiment.getScore}, Magnitude: ${s.getSentiment.getMagnitude}) ${s.getText.getContent}" }
      .collect(Collectors.joining("\n"))

    s"""
       |File Name: $fileName
       |Most impactful sentence: $mostImpactfulSentence
       |The general tone of the text is $toneOfText.
       |Language: ${sentiment.getLanguage}
       |
       |$x
       |
    """.stripMargin

  }
}
