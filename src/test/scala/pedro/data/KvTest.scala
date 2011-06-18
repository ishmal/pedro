


package pedro.data

import java.util.Date

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FeatureSpec,GivenWhenThen}

import pedro.data.{Data,Schema,StringIndex,Kind,JdbcKvStore}



@RunWith(classOf[JUnitRunner])
class KvStoreTest extends FeatureSpec with GivenWhenThen
{
	case class Item(
	    val id : String  = "",
	    val sval: String = "",
	    val ival: Int    = 0,
	    val lval: Long   = 0L,
	    val dval: Double = 0.0,
	    val dateval: Date = new Date,
	    val tags : Seq[String] = Seq()
	) extends pedro.data.Data
	{
	  def this(js: JsonValue) = this(
            id=js("id"),
            sval=js("sval"),
            ival=js("ival"),
            lval=js("lval"),
            dval=js("dval"),
            dateval=js("dateval"),
            tags=js("tags")
	        )
	}
	
	object TestSchema extends Schema
	{
	    val items = new Kind("items")(js=> Item(js))
	        {
	        add(this)
	        val byName   = stringIndex("name")(js=>js("name"))
	        val byStr    = stringIndex("sval")(js=>js("sval"))
	        val byInt    = intIndex("ival")(js=>js("ival"))
	        val byLong   = longIndex("lval")(js=>js("lval"))
	        val byDouble = doubleIndex("dval")(js=>js("dval"))
	        val byDate   = dateIndex("dateval")(js=>js("dateval"))
	        val byTag    = stringIndex("tags")(js=>js("tags"))
	        }
	}

import TestSchema._

    feature("The KeyValue store can put/get/query and delete records on a backend")
        {
        scenario("The store should be able to connect and disconnect")
            {
            given("a JDBC KV Store")
            val store = new JdbcKvStore(Map("url" -> "jdbc:h2:~/pedro"))
            when("connect is called")
            var result = store.connect
            then("the return value should be true")
            assert(result)
            when("create is called")
            result = store.create(TestSchema)
            then("the return value should be true")
            assert(result)

            when("a new Item is added")
            val item1 = new Item(id="rwj", tags=Array("big","tall"))
            result = store.put(items,item1)
            then("the return value should be true")
            assert(result)

            when("a new Item is added")
            val item2 = new Item(id="jd", tags=Array("handsome","gregarious"))
            result = store.put(items,item2)
            then("the return value should be true")
            assert(result)

            when("a new Item is added")
            val item3 = new Item(id="kb", tags=Array("dance","fever"))
            result = store.put(items,item3)
            then("the return value should be true")
            assert(result)

            when("a item is queried")
            val xs = store.query(items, items.byTag, {s:String=> s=="big"})
            then("the return value should be > 0")
            println("1#########"); xs.get.foreach(println)
            assert(xs.isDefined)
            assert(xs.get.size > 0)

            when("a item is deleted")
            result = store.delete(items, "rwj")
            then("the return value should be true")
            assert(result)
            
            when("2 a item is queried after deletion")
            val xs2 = store.query(items, items.byTag, {s:String=> s=="big"})
            then("the return value should be == 0")
            println("2######### "+ xs2.size); xs2.get.foreach(println)
            assert(xs2.isDefined)
            expect(0)(xs2.get.size)

            }

    
        }

}


