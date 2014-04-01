Getting started with bintray


Upload a jar (replace yfcai by your own username):
```
curl --user yfcai:API_KEY --upload-file Conversion-Macro-0.1.jar 'https://api.bintray.com/content/yfcai/maven/Conversion-Macro/0.1/com/mueller/Conversion-Macro/0.1/Conversion-Macro-0.1.jar'
```

Upload a pom (required to resolve via sbt):
```
curl --user yfcai:API_KEY --upload-file Conversion-Macro-0.1.pom 'https://api.bintray.com/content/yfcai/maven/Conversion-Macro/0.1/com/mueller/Conversion-Macro/0.1/Conversion-Macro-0.1.pom'
```


Reference

1. Bintray REST API
   https://bintray.com/docs/api.html

2. Bintray help (How do I get the API key? By editing my profile, of course! (search for "API keys"))
   https://bintray.com/docs/bintrayuserguide.html
