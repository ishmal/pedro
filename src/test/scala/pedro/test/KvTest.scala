


package pedro.test

import pedro.data.{Data,Schema,StringIndex,Kind,JdbcKvStore}

import org.scalatest.{FeatureSpec,GivenWhenThen}
import org.scalatest.matchers.MustMatchers
 

case class User(
    val id : String = "",
    val name: String = "",
    val tags : Array[String] = Array()
) extends pedro.data.Data
{}

object TestSchema extends Schema
{
    val users = new Kind[User]("users")(js=>
        {
        User(
            id=js("id"),
            name=js("name")
            )
        })
        {
        kbuf.append(this)
        val byName = stringIndex("name")(js=>js("name"))
        val byTag  = stringIndex("tags")(js=>js("tags"))
        }
}


import TestSchema._

class KvStoreSpec extends FeatureSpec with GivenWhenThen with MustMatchers
{
    feature("The KeyValue store can put/get/query and delete records on a backend")
        {
        scenario("The store should be able to connect and disconnect")
            {
            given("a JDBC KV Store")
            val store = new JdbcKvStore(jdbcUrl="jdbc:h2:~/pedro")
            when("connect is called")
            var result = store.connect
            then("the return value should be true")
            result must be === true
            when("create is called")
            result = store.create(TestSchema)
            then("the return value should be true")
            result must be === true

            when("a new User is added")
            val user1 = new User(id="rwj", tags=Array("big","tall"))
            result = store.put(users,user1)
            then("the return value should be true")
            result must be === true

            when("a new User is added")
            val user2 = new User(id="jd", tags=Array("handsome","gregarious"))
            result = store.put(users,user2)
            then("the return value should be true")
            result must be === true

            when("a new User is added")
            val user3 = new User(id="kb", tags=Array("dance","fever"))
            result = store.put(users,user3)
            then("the return value should be true")
            result must be === true

            when("a user is queried")
            val xs = store.query(users, users.byTag, {s:String=> s=="big"})
            then("the return value should be > 0")
            println("1#########"); xs.get.foreach(println)
            xs.isDefined must be === true
            xs.get.size must be > 0

            when("a user is deleted")
            result = store.delete(users, "rwj")
            then("the return value should be true")
            result must be === true
            
            when("2 a user is queried after deletion")
            val xs2 = store.query(users, users.byTag, {s:String=> s=="big"})
            then("the return value should be == 0")
            println("2######### "+ xs2.size); xs2.get.foreach(println)
            xs2.isDefined must be === true
            xs2.get.size must be === 0

            }

    
        }

}


