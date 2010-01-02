message = {
	"_id": "_design/message",
	"language": "javascript",
	"views": {
		"undelivered": {
			"map" : "function(doc) {\n if (doc.type == 'message' && !doc.consumed_timestamp) {\nemit([doc.destination, doc.created_timestamp], doc);\n}\n}"
		}
	
	}
	
}
