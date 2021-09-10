package com.itechart.json

import io.circe._
import io.circe.generic.JsonCodec
import io.circe.parser._
import io.circe.syntax._

import io.circe.generic.extras._
import io.circe.generic.extras.semiauto._

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
    val postList = decode[List[Post]](TestData.posts)
    println(postList)
    println(postList.getOrElse(List.empty).asJson)
    val commentList = decode[List[Comment]](TestData.comments)
    println(commentList)
    println(commentList.getOrElse(List.empty).asJson)
    val albumList = decode[List[Album]](TestData.albums)
    println(albumList)
    println(albumList.getOrElse(List.empty).asJson)
    val todoList = decode[List[Todo]](TestData.todos)
    println(todoList)
    println(todoList.getOrElse(List.empty).asJson)
    val userList = decode[List[User]](TestData.users)
    println(userList)
    println(userList.getOrElse(List.empty).asJson)
  }

}
