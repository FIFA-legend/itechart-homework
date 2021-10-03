package com.itechart.json

import io.circe._
import io.circe.generic.JsonCodec
import io.circe.parser._
import io.circe.syntax._
import io.circe.generic.extras._
import io.circe.generic.extras.semiauto._

import scala.io.Source

object Circe {

  @JsonCodec final case class Geo(lat: String, lng: String)

  @JsonCodec final case class Address(
    street:  String,
    suite:   String,
    city:    String,
    zipcode: String,
    geo:     Geo
  )

  @JsonCodec final case class Company(
    name:        String,
    catchPhrase: String,
    bs:          String
  )

  @JsonCodec final case class User(
    id:       Long,
    name:     String,
    username: String,
    email:    String,
    address:  Address,
    phone:    String,
    website:  String,
    company:  Company
  )

  @JsonCodec final case class Post(
    userId: Long,
    id:     Long,
    title:  String,
    body:   String
  )

  final case class Album(
    userId: Long,
    id:     Long,
    title:  String
  )

  final case class Todo(
    userId:    Long,
    id:        Long,
    title:     String,
    completed: Boolean
  )

  final case class Comment(
    postId: Long,
    id:     Long,
    name:   String,
    email:  String,
    body:   String
  )

  implicit val config: Configuration = Configuration.default

  implicit val albumEncoder: Encoder[Album] = deriveConfiguredEncoder[Album]
  implicit val albumDecoder: Decoder[Album] = deriveConfiguredDecoder[Album]
  implicit val todoCodec:    Codec[Todo]    = deriveConfiguredCodec[Todo]
  implicit val commentEncoder: Encoder[Comment] =
    Encoder.forProduct5("postId", "id", "name", "email", "body")(c => (c.postId, c.id, c.name, c.email, c.body))
  implicit val commentDecoder: Decoder[Comment] =
    Decoder.forProduct5("postId", "id", "name", "email", "body")(Comment)

  def main(args: Array[String]): Unit = {
    val postSource = Source.fromURL("https://jsonplaceholder.typicode.com/posts")
    val rawPosts   = postSource.getLines().foldLeft("")(_ + _)
    val postList   = decode[List[Post]](rawPosts)
    println(postList)
    println(postList.getOrElse(List.empty).asJson)
    postSource.close()

    val commentSource = Source.fromURL("https://jsonplaceholder.typicode.com/comments")
    val rawComments   = commentSource.getLines().foldLeft("")(_ + _)
    val commentList   = decode[List[Comment]](rawComments)
    println(commentList)
    println(commentList.getOrElse(List.empty).asJson)
    commentSource.close()

    val albumSource = Source.fromURL("https://jsonplaceholder.typicode.com/albums")
    val rawAlbums   = albumSource.getLines().foldLeft("")(_ + _)
    val albumList   = decode[List[Album]](rawAlbums)
    println(albumList)
    println(albumList.getOrElse(List.empty).asJson)
    albumSource.close()

    val todoSource = Source.fromURL("https://jsonplaceholder.typicode.com/todos")
    val rawTodos   = todoSource.getLines().foldLeft("")(_ + _)
    val todoList   = decode[List[Todo]](rawTodos)
    println(todoList)
    println(todoList.getOrElse(List.empty).asJson)
    todoSource.close()

    val userSource = Source.fromURL("https://jsonplaceholder.typicode.com/users")
    val rawUsers   = userSource.getLines().foldLeft("")(_ + _)
    val userList   = decode[List[User]](rawUsers)
    println(userList)
    println(userList.getOrElse(List.empty).asJson)
    userSource.close()
  }

}
