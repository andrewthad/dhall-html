[ nodeElement
  "div"
  [{name = "class",value = "drew"}]
  (List/reverse Node [nodeText " hey", nodeText "there"])
, nodeElement
  "span"
  [{name = "id",value = "main-form"}]
  [nodeText "I am the text"]
] # ( ./content.html )
