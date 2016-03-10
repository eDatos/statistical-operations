
<!DOCTYPE html>
<html>
<head>
  <meta charset="UTF-8">
  <title>Statistical Operations API</title>
  <link rel="icon" type="image/png" href="images/favicon-32x32.png" sizes="32x32" />
  <link rel="icon" type="image/png" href="images/favicon-16x16.png" sizes="16x16" />
  <link href="<%=org.siemac.metamac.statistical.operations.web.external.WebUtils.getApiBaseURL()%>/swagger-ui/css/typography.css" media='screen' rel='stylesheet' type='text/css'/>
  <link href="<%=org.siemac.metamac.statistical.operations.web.external.WebUtils.getApiBaseURL()%>/swagger-ui/css/reset.css" media='screen' rel='stylesheet' type='text/css'/>
  <link href="<%=org.siemac.metamac.statistical.operations.web.external.WebUtils.getApiBaseURL()%>/swagger-ui/css/screen.css" media='screen' rel='stylesheet' type='text/css'/>
  <link href="<%=org.siemac.metamac.statistical.operations.web.external.WebUtils.getApiBaseURL()%>/swagger-ui/css/reset.css" media='print' rel='stylesheet' type='text/css'/>
  <link href="<%=org.siemac.metamac.statistical.operations.web.external.WebUtils.getApiBaseURL()%>/swagger-ui/css/print.css" media='print' rel='stylesheet' type='text/css'/>
  <link href="<%=org.siemac.metamac.statistical.operations.web.external.WebUtils.getFavicon()%>" rel="shortcut icon"/>
  <script src="<%=org.siemac.metamac.statistical.operations.web.external.WebUtils.getApiBaseURL()%>/swagger-ui/lib/jquery-1.8.0.min.js" type='text/javascript'></script>
  <script src="<%=org.siemac.metamac.statistical.operations.web.external.WebUtils.getApiBaseURL()%>/swagger-ui/lib/jquery.slideto.min.js" type='text/javascript'></script>
  <script src="<%=org.siemac.metamac.statistical.operations.web.external.WebUtils.getApiBaseURL()%>/swagger-ui/lib/jquery.wiggle.min.js" type='text/javascript'></script>
  <script src="<%=org.siemac.metamac.statistical.operations.web.external.WebUtils.getApiBaseURL()%>/swagger-ui/lib/jquery.ba-bbq.min.js" type='text/javascript'></script>
  <script src="<%=org.siemac.metamac.statistical.operations.web.external.WebUtils.getApiBaseURL()%>/swagger-ui/lib/handlebars-2.0.0.js" type='text/javascript'></script>
  <script src="<%=org.siemac.metamac.statistical.operations.web.external.WebUtils.getApiBaseURL()%>/swagger-ui/lib/underscore-min.js" type='text/javascript'></script>
  <script src="<%=org.siemac.metamac.statistical.operations.web.external.WebUtils.getApiBaseURL()%>/swagger-ui/lib/backbone-min.js" type='text/javascript'></script>
  <script src="<%=org.siemac.metamac.statistical.operations.web.external.WebUtils.getApiBaseURL()%>/swagger-ui/swagger-ui.js" type='text/javascript'></script>
  <script src="<%=org.siemac.metamac.statistical.operations.web.external.WebUtils.getApiBaseURL()%>/swagger-ui/lib/highlight.7.3.pack.js" type='text/javascript'></script>
  <script src="<%=org.siemac.metamac.statistical.operations.web.external.WebUtils.getApiBaseURL()%>/swagger-ui/lib/jsoneditor.min.js" type='text/javascript'></script>
  <script src="<%=org.siemac.metamac.statistical.operations.web.external.WebUtils.getApiBaseURL()%>/swagger-ui/lib/marked.js" type='text/javascript'></script>
  <script src="<%=org.siemac.metamac.statistical.operations.web.external.WebUtils.getApiBaseURL()%>/swagger-ui/lib/swagger-oauth.js" type='text/javascript'></script>

  <!-- Some basic translations -->
  <script src="<%=org.siemac.metamac.statistical.operations.web.external.WebUtils.getApiBaseURL()%>/swagger-ui/lang/translator.js" type='text/javascript'></script>
  <script src="<%=org.siemac.metamac.statistical.operations.web.external.WebUtils.getApiBaseURL()%>/swagger-ui/lang/es.js" type='text/javascript'></script>

  <script type="text/javascript">
    $(function () {
      var url = window.location.search.match(/url=([^&]+)/);
      if (url && url.length > 1) {
        url = decodeURIComponent(url[1]);
      } else {
    	  var baseUrl = document.location.href;
          //this removes the anchor at the end, if there is one
          baseUrl = baseUrl.substring(0, (baseUrl.indexOf("#") == -1) ? baseUrl.length : baseUrl.indexOf("#"));
          //this removes the query after the file name, if there is one
          baseUrl = baseUrl.substring(0, (baseUrl.indexOf("?") == -1) ? baseUrl.length : baseUrl.indexOf("?"));
          //this removes everything after the last slash in the path
          baseUrl = baseUrl.substring(0, (baseUrl.lastIndexOf("/") == -1) ? baseUrl.length : baseUrl.lastIndexOf("/"));

          url = baseUrl + "/swagger.jsp";
      }

      // Pre load translate...
      if(window.SwaggerTranslator) {
        window.SwaggerTranslator.translate();
      }
      window.swaggerUi = new SwaggerUi({
        url: url,
        dom_id: "swagger-ui-container",
        supportedSubmitMethods: ['get', 'post', 'put', 'delete', 'patch'],
        onComplete: function(swaggerApi, swaggerUi){
          if(typeof initOAuth == "function") {
            initOAuth({
              clientId: "your-client-id",
              clientSecret: "your-client-secret-if-required",
              realm: "your-realms",
              appName: "your-app-name", 
              scopeSeparator: ",",
              additionalQueryStringParams: {}
            });
          }

          if(window.SwaggerTranslator) {
            window.SwaggerTranslator.translate();
          }

          $('pre code').each(function(i, e) {
            hljs.highlightBlock(e)
          });

          addApiKeyAuthorization();
        },
        onFailure: function(data) {
          log("Unable to Load SwaggerUI");
        },
        docExpansion: "none",
        jsonEditor: false,
        apisSorter: "alpha",
        defaultModelRendering: 'model',
        showRequestHeaders: false
      });

      function addApiKeyAuthorization(){
        var key = encodeURIComponent($('#input_apiKey')[0].value);
        if(key && key.trim() != "") {
            var apiKeyAuth = new SwaggerClient.ApiKeyAuthorization("api_key", key, "query");
            window.swaggerUi.api.clientAuthorizations.add("api_key", apiKeyAuth);
            log("added key " + key);
        }
      }

      $('#input_apiKey').change(addApiKeyAuthorization);

      // if you have an apiKey you would like to pre-populate on the page for demonstration purposes...
      /*
        var apiKey = "myApiKeyXXXX123456789";
        $('#input_apiKey').val(apiKey);
      */

      window.swaggerUi.load();

      function log() {
        if ('console' in window) {
          console.log.apply(console, arguments);
        }
      }
  });
  </script>
</head>

<body class="swagger-section">

<div id="message-bar" class="swagger-ui-wrap" data-sw-translate>&nbsp;</div>
<div id="swagger-ui-container" class="swagger-ui-wrap"></div>
</body>
</html>
