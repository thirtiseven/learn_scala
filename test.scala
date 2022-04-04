object Untitled {
	def main(args: Array[String]) = {
		var caapital = Map("US" -> "Washington", "France" -> "Paris")
		caapital += ("Japan" -> "Tokyo")
		println(caapital("France"))
	}
}
