object BasicError extends App {

	def throwError(msg: String): Unit = {
		error(msg)
	}

	throwError("basic error test")
}