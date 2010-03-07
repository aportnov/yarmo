message = {
	"_id": "_design/message",
	"language": "javascript",
	"views": {
		"undelivered": {
			"map" : "function(doc) {\n if (doc.type == 'message' && !doc.consumed_timestamp) {\nemit([doc.destination, doc.created_timestamp, doc._id], doc);\n}\n}"
		},
		"unacknowledged": {
			"map" : "function(doc) {\n if (doc.type == 'message' && doc.destination.split(':')[0] == 'queue' && doc.consumed_timestamp && !doc.acknowledged_timestamp) {\nemit([doc.destination, doc.consumed_timestamp, doc._id], null); \n} \n}"
		},
		"all": {
			"map" : "function(doc) {\n if (doc.type == 'message') {\nemit([doc.destination, doc.created_timestamp], doc);\n} \n}"
		}
	
	}
	
}

destination = {
	"_id": "_design/destination",
	"language": "javascript",
	"views": {
		"all": {
			"map": "function(doc){\n if(doc.type == 'queue' || doc.type == 'topic') {\nemit([doc.type, doc.name], doc); \n} \n}"
		},
		"subscribers" : {
			"map": "function(doc){\n if(doc.type == 'subscription'){\nemit([doc.destination], doc); \n}\n}"
		}
	}
}