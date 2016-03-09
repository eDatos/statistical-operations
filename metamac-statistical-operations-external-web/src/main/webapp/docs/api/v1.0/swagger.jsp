{
  "swagger": "2.0",
  "info" : {
    "description" : "Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aenean commodo ligula eget dolor. Aenean massa. Cum sociis natoque penatibus et magnis\n\t\tdis parturient montes, nascetur ridiculus mus. Donec quam felis, ultricies nec, pellentesque eu, pretium quis, sem. Nulla consequat massa quis enim. Donec\n\t\tpede justo, fringilla vel, aliquet nec, vulputate eget, arcu. In enim justo, rhoncus ut, imperdiet a, venenatis vitae, justo. Nullam dictum felis eu pede\n\t\tmollis pretium. Integer tincidunt. Cras dapibus. Vivamus elementum semper nisi. Aenean vulputate eleifend tellus. Aenean leo ligula, porttitor eu, consequat\n\t\tvitae, eleifend ac, enim. Aliquam lorem ante, dapibus in, viverra quis, feugiat a, tellus. Phasellus viverra nulla ut metus varius laoreet. Quisque rutrum.\n\t\tAenean imperdiet. Etiam ultricies nisi vel augue. Curabitur ullamcorper ultricies nisi. Nam eget dui. Etiam rhoncus. Maecenas tempus, tellus eget condimentum\n\t\trhoncus, sem quam semper libero, sit amet adipiscing sem neque sed ipsum. Nam quam nunc, blandit vel, luctus pulvinar, hendrerit id, lorem. Maecenas nec odio\n\t\tet ante tincidunt tempus. Donec vitae sapien ut libero venenatis faucibus. Nullam quis ante. Etiam sit amet orci eget eros faucibus tincidunt. Duis leo. Sed\n\t\tfringilla mauris sit amet nibh. Donec sodales sagittis magna. Sed consequat, leo eget bibendum sodales, augue velit cursus nunc,",
    "version" : "1.7.2-SNAPSHOT",
    "title" : "API de operaciones estadísticas"
  },
  "host" : "<%=org.siemac.metamac.statistical.operations.web.external.WebUtils.getBaseURL()%>",
  "schemes" : [],
  "tags" : [
    {
      "name" : "StatisticalOperationsV1_0",
      "description" : ""
    }
  ],
  "definitions" : {
    "xml_operations_ClassSystems" : {
      "type" : "object",
      "title" : "ClassSystems",
      "allOf" : [
        {
          "$ref" : "#/definitions/xml_cdomain_ListBase"
        },
        {
          "properties" : {
            "classSystem" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_Resource"
            }
          }
        }
      ],
      "description" : "<p>Java class for ClassSystems complex type.\r\n\r\n<p>The following schema fragment specifies the expected content contained within this class.\r\n\r\n<pre>\r\n &lt;complexType name=\"ClassSystems\">\r\n   &lt;complexContent>\r\n     &lt;extension base=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}ListBase\">\r\n       &lt;sequence>\r\n         &lt;element name=\"classSystem\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}Resource\" maxOccurs=\"unbounded\" minOccurs=\"0\"/>\r\n       &lt;/sequence>\r\n     &lt;/extension>\r\n   &lt;/complexContent>\r\n &lt;/complexType>\r\n <\/pre>"
    }
    ,
    "xml_operations_CollMethods" : {
      "type" : "object",
      "title" : "CollMethods",
      "allOf" : [
        {
          "$ref" : "#/definitions/xml_cdomain_ListBase"
        },
        {
          "properties" : {
            "collMethod" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_Item"
            }
          }
        }
      ],
      "description" : "<p>Java class for CollMethods complex type.\r\n\r\n<p>The following schema fragment specifies the expected content contained within this class.\r\n\r\n<pre>\r\n &lt;complexType name=\"CollMethods\">\r\n   &lt;complexContent>\r\n     &lt;extension base=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}ListBase\">\r\n       &lt;sequence>\r\n         &lt;element name=\"collMethod\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}Item\" maxOccurs=\"unbounded\" minOccurs=\"0\"/>\r\n       &lt;/sequence>\r\n     &lt;/extension>\r\n   &lt;/complexContent>\r\n &lt;/complexType>\r\n <\/pre>"
    }
    ,
    "xml_operations_Costs" : {
      "type" : "object",
      "title" : "Costs",
      "allOf" : [
        {
          "$ref" : "#/definitions/xml_cdomain_ListBase"
        },
        {
          "properties" : {
            "cost" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_Item"
            }
          }
        }
      ],
      "description" : "<p>Java class for Costs complex type.\r\n\r\n<p>The following schema fragment specifies the expected content contained within this class.\r\n\r\n<pre>\r\n &lt;complexType name=\"Costs\">\r\n   &lt;complexContent>\r\n     &lt;extension base=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}ListBase\">\r\n       &lt;sequence>\r\n         &lt;element name=\"cost\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}Item\" maxOccurs=\"unbounded\" minOccurs=\"0\"/>\r\n       &lt;/sequence>\r\n     &lt;/extension>\r\n   &lt;/complexContent>\r\n &lt;/complexType>\r\n <\/pre>"
    }
    ,
    "xml_operations_DataSharings" : {
      "type" : "object",
      "title" : "DataSharings",
      "allOf" : [
        {
          "$ref" : "#/definitions/xml_cdomain_ListBase"
        },
        {
          "properties" : {
            "dataSharing" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_InternationalString"
            }
          }
        }
      ],
      "description" : "<p>Java class for DataSharings complex type.\r\n\r\n<p>The following schema fragment specifies the expected content contained within this class.\r\n\r\n<pre>\r\n &lt;complexType name=\"DataSharings\">\r\n   &lt;complexContent>\r\n     &lt;extension base=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}ListBase\">\r\n       &lt;sequence>\r\n         &lt;element name=\"dataSharing\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}InternationalString\" maxOccurs=\"unbounded\" minOccurs=\"0\"/>\r\n       &lt;/sequence>\r\n     &lt;/extension>\r\n   &lt;/complexContent>\r\n &lt;/complexType>\r\n <\/pre>"
    }
    ,
    "xml_operations_Families" : {
      "type" : "object",
      "title" : "Families",
      "allOf" : [
        {
          "$ref" : "#/definitions/xml_cdomain_ListBase"
        },
        {
          "properties" : {
            "family" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_Resource"
            }
          }
        }
      ],
      "description" : "<p>Java class for Families complex type.\r\n\r\n<p>The following schema fragment specifies the expected content contained within this class.\r\n\r\n<pre>\r\n &lt;complexType name=\"Families\">\r\n   &lt;complexContent>\r\n     &lt;extension base=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}ListBase\">\r\n       &lt;sequence>\r\n         &lt;element name=\"family\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}Resource\" maxOccurs=\"unbounded\" minOccurs=\"0\"/>\r\n       &lt;/sequence>\r\n     &lt;/extension>\r\n   &lt;/complexContent>\r\n &lt;/complexType>\r\n <\/pre>"
    }
    ,
    "xml_operations_Family" : {
      "type" : "object",
      "title" : "Family",
      "allOf" : [
        {
          "properties" : {
            "kind" : {
              "xml" : {
                "attribute" : true,
                "namespace" : ""
              },
"description" : "",
"type" : "string"
            },
            "acronym" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_InternationalString"
            },
            "childLinks" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_ChildLinks"
            },
            "description" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_InternationalString"
            },
            "id" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"type" : "string"
            },
            "inventoryDate" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"type" : "string"
            },
            "name" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_InternationalString"
            },
            "parentLink" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_ResourceLink"
            },
            "selfLink" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_ResourceLink"
            },
            "urn" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"type" : "string"
            }
          }
        }
      ],
      "description" : "<p>Java class for Family complex type.\r\n\r\n<p>The following schema fragment specifies the expected content contained within this class.\r\n\r\n<pre>\r\n &lt;complexType name=\"Family\">\r\n   &lt;complexContent>\r\n     &lt;restriction base=\"{http://www.w3.org/2001/XMLSchema}anyType\">\r\n       &lt;sequence>\r\n         &lt;element name=\"id\" type=\"{http://www.w3.org/2001/XMLSchema}string\"/>\r\n         &lt;element name=\"urn\" type=\"{http://www.w3.org/2001/XMLSchema}string\"/>\r\n         &lt;element name=\"selfLink\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}ResourceLink\"/>\r\n         &lt;element name=\"parentLink\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}ResourceLink\"/>\r\n         &lt;element name=\"childLinks\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}ChildLinks\"/>\r\n         &lt;element name=\"name\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}InternationalString\"/>\r\n         &lt;element name=\"acronym\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}InternationalString\" minOccurs=\"0\"/>\r\n         &lt;element name=\"description\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}InternationalString\" minOccurs=\"0\"/>\r\n         &lt;element name=\"inventoryDate\" type=\"{http://www.w3.org/2001/XMLSchema}dateTime\" minOccurs=\"0\"/>\r\n       &lt;/sequence>\r\n       &lt;attribute name=\"kind\" use=\"required\" type=\"{http://www.w3.org/2001/XMLSchema}string\" />\r\n     &lt;/restriction>\r\n   &lt;/complexContent>\r\n &lt;/complexType>\r\n <\/pre>"
    }
    ,
    "xml_operations_FreqColls" : {
      "type" : "object",
      "title" : "FreqColls",
      "allOf" : [
        {
          "$ref" : "#/definitions/xml_cdomain_ListBase"
        },
        {
          "properties" : {
            "freqColl" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_Resource"
            }
          }
        }
      ],
      "description" : "<p>Java class for FreqColls complex type.\r\n\r\n<p>The following schema fragment specifies the expected content contained within this class.\r\n\r\n<pre>\r\n &lt;complexType name=\"FreqColls\">\r\n   &lt;complexContent>\r\n     &lt;extension base=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}ListBase\">\r\n       &lt;sequence>\r\n         &lt;element name=\"freqColl\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}Resource\" maxOccurs=\"unbounded\" minOccurs=\"0\"/>\r\n       &lt;/sequence>\r\n     &lt;/extension>\r\n   &lt;/complexContent>\r\n &lt;/complexType>\r\n <\/pre>"
    }
    ,
    "xml_operations_GeographicGranularities" : {
      "type" : "object",
      "title" : "GeographicGranularities",
      "allOf" : [
        {
          "$ref" : "#/definitions/xml_cdomain_ListBase"
        },
        {
          "properties" : {
            "geographicGranularity" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_Resource"
            }
          }
        }
      ],
      "description" : "<p>Java class for GeographicGranularities complex type.\r\n\r\n<p>The following schema fragment specifies the expected content contained within this class.\r\n\r\n<pre>\r\n &lt;complexType name=\"GeographicGranularities\">\r\n   &lt;complexContent>\r\n     &lt;extension base=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}ListBase\">\r\n       &lt;sequence>\r\n         &lt;element name=\"geographicGranularity\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}Resource\" maxOccurs=\"unbounded\" minOccurs=\"0\"/>\r\n       &lt;/sequence>\r\n     &lt;/extension>\r\n   &lt;/complexContent>\r\n &lt;/complexType>\r\n <\/pre>"
    }
    ,
    "xml_operations_InformationSuppliers" : {
      "type" : "object",
      "title" : "InformationSuppliers",
      "allOf" : [
        {
          "$ref" : "#/definitions/xml_cdomain_ListBase"
        },
        {
          "properties" : {
            "informationSupplier" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_Resource"
            }
          }
        }
      ],
      "description" : "<p>Java class for InformationSuppliers complex type.\r\n\r\n<p>The following schema fragment specifies the expected content contained within this class.\r\n\r\n<pre>\r\n &lt;complexType name=\"InformationSuppliers\">\r\n   &lt;complexContent>\r\n     &lt;extension base=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}ListBase\">\r\n       &lt;sequence>\r\n         &lt;element name=\"informationSupplier\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}Resource\" maxOccurs=\"unbounded\" minOccurs=\"0\"/>\r\n       &lt;/sequence>\r\n     &lt;/extension>\r\n   &lt;/complexContent>\r\n &lt;/complexType>\r\n <\/pre>"
    }
    ,
    "xml_operations_Instance" : {
      "type" : "object",
      "title" : "Instance",
      "allOf" : [
        {
          "properties" : {
            "kind" : {
              "xml" : {
                "attribute" : true,
                "namespace" : ""
              },
"description" : "",
"type" : "string"
            },
            "accuracyOverall" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_InternationalString"
            },
            "acronym" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_InternationalString"
            },
            "adjustment" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_InternationalString"
            },
            "basePeriod" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"type" : "string"
            },
            "childLinks" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_ChildLinks"
            },
            "classSystems" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_operations_ClassSystems"
            },
            "classSystemsDescription" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_InternationalString"
            },
            "coherInternal" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_InternationalString"
            },
            "coherXDom" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_InternationalString"
            },
            "collMethod" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_Item"
            },
            "comment" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_InternationalString"
            },
            "completeness" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_InternationalString"
            },
            "dataCompilation" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_InternationalString"
            },
            "dataDescription" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_InternationalString"
            },
            "dataValidation" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_InternationalString"
            },
            "docMethod" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_InternationalString"
            },
            "freqColls" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_operations_FreqColls"
            },
            "geographicComparability" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_InternationalString"
            },
            "geographicGranularity" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_operations_GeographicGranularities"
            },
            "id" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"type" : "string"
            },
            "informationSuppliers" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_operations_InformationSuppliers"
            },
            "inventoryDate" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"type" : "string"
            },
            "measures" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_operations_Measures"
            },
            "name" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_InternationalString"
            },
            "nonsamplingErr" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_InternationalString"
            },
            "parentLink" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_ResourceLink"
            },
            "predecessor" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_Resource"
            },
            "punctuality" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_InternationalString"
            },
            "qualityAssmnt" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_InternationalString"
            },
            "qualityAssure" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_InternationalString"
            },
            "qualityDoc" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_InternationalString"
            },
            "samplingErr" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_InternationalString"
            },
            "selfLink" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_ResourceLink"
            },
            "statConcDefs" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_operations_StatConcDefs"
            },
            "statConcDefsDescription" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_InternationalString"
            },
            "statisticalOperation" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_Resource"
            },
            "statisticalOperationSource" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_Item"
            },
            "statisticalPopulation" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_InternationalString"
            },
            "statisticalUnits" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_operations_StatisticalUnits"
            },
            "successor" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_Resource"
            },
            "temporalComparability" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_InternationalString"
            },
            "temporalGranularity" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_operations_TemporalGranularities"
            },
            "timeliness" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_InternationalString"
            },
            "urn" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"type" : "string"
            },
            "userNeeds" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_InternationalString"
            },
            "userSat" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_InternationalString"
            }
          }
        }
      ],
      "description" : "<p>Java class for Instance complex type.\r\n\r\n<p>The following schema fragment specifies the expected content contained within this class.\r\n\r\n<pre>\r\n &lt;complexType name=\"Instance\">\r\n   &lt;complexContent>\r\n     &lt;restriction base=\"{http://www.w3.org/2001/XMLSchema}anyType\">\r\n       &lt;sequence>\r\n         &lt;element name=\"id\" type=\"{http://www.w3.org/2001/XMLSchema}string\"/>\r\n         &lt;element name=\"urn\" type=\"{http://www.w3.org/2001/XMLSchema}string\"/>\r\n         &lt;element name=\"selfLink\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}ResourceLink\"/>\r\n         &lt;element name=\"parentLink\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}ResourceLink\"/>\r\n         &lt;element name=\"childLinks\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}ChildLinks\" minOccurs=\"0\"/>\r\n         &lt;element name=\"name\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}InternationalString\"/>\r\n         &lt;element name=\"acronym\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}InternationalString\" minOccurs=\"0\"/>\r\n         &lt;element name=\"statisticalOperation\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}Resource\"/>\r\n         &lt;element name=\"successor\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}Resource\" minOccurs=\"0\"/>\r\n         &lt;element name=\"predecessor\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}Resource\" minOccurs=\"0\"/>\r\n         &lt;element name=\"dataDescription\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}InternationalString\" minOccurs=\"0\"/>\r\n         &lt;element name=\"statisticalPopulation\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}InternationalString\" minOccurs=\"0\"/>\r\n         &lt;element name=\"statisticalUnits\" type=\"{http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain}StatisticalUnits\" minOccurs=\"0\"/>\r\n         &lt;element name=\"geographicGranularity\" type=\"{http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain}GeographicGranularities\" minOccurs=\"0\"/>\r\n         &lt;element name=\"geographicComparability\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}InternationalString\" minOccurs=\"0\"/>\r\n         &lt;element name=\"temporalGranularity\" type=\"{http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain}TemporalGranularities\" minOccurs=\"0\"/>\r\n         &lt;element name=\"temporalComparability\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}InternationalString\" minOccurs=\"0\"/>\r\n         &lt;element name=\"basePeriod\" type=\"{http://www.w3.org/2001/XMLSchema}string\" minOccurs=\"0\"/>\r\n         &lt;element name=\"measures\" type=\"{http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain}Measures\" minOccurs=\"0\"/>\r\n         &lt;element name=\"statConcDefsDescription\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}InternationalString\" minOccurs=\"0\"/>\r\n         &lt;element name=\"statConcDefs\" type=\"{http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain}StatConcDefs\" minOccurs=\"0\"/>\r\n         &lt;element name=\"classSystemsDescription\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}InternationalString\" minOccurs=\"0\"/>\r\n         &lt;element name=\"classSystems\" type=\"{http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain}ClassSystems\" minOccurs=\"0\"/>\r\n         &lt;element name=\"docMethod\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}InternationalString\" minOccurs=\"0\"/>\r\n         &lt;element name=\"statisticalOperationSource\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}Item\" minOccurs=\"0\"/>\r\n         &lt;element name=\"collMethod\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}Item\" minOccurs=\"0\"/>\r\n         &lt;element name=\"informationSuppliers\" type=\"{http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain}InformationSuppliers\" minOccurs=\"0\"/>\r\n         &lt;element name=\"freqColls\" type=\"{http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain}FreqColls\" minOccurs=\"0\"/>\r\n         &lt;element name=\"dataValidation\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}InternationalString\" minOccurs=\"0\"/>\r\n         &lt;element name=\"dataCompilation\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}InternationalString\" minOccurs=\"0\"/>\r\n         &lt;element name=\"adjustment\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}InternationalString\" minOccurs=\"0\"/>\r\n         &lt;element name=\"inventoryDate\" type=\"{http://www.w3.org/2001/XMLSchema}dateTime\" minOccurs=\"0\"/>\r\n         &lt;element name=\"qualityDoc\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}InternationalString\" minOccurs=\"0\"/>\r\n         &lt;element name=\"qualityAssure\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}InternationalString\" minOccurs=\"0\"/>\r\n         &lt;element name=\"qualityAssmnt\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}InternationalString\" minOccurs=\"0\"/>\r\n         &lt;element name=\"userNeeds\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}InternationalString\" minOccurs=\"0\"/>\r\n         &lt;element name=\"userSat\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}InternationalString\" minOccurs=\"0\"/>\r\n         &lt;element name=\"completeness\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}InternationalString\" minOccurs=\"0\"/>\r\n         &lt;element name=\"timeliness\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}InternationalString\" minOccurs=\"0\"/>\r\n         &lt;element name=\"punctuality\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}InternationalString\" minOccurs=\"0\"/>\r\n         &lt;element name=\"accuracyOverall\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}InternationalString\" minOccurs=\"0\"/>\r\n         &lt;element name=\"samplingErr\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}InternationalString\" minOccurs=\"0\"/>\r\n         &lt;element name=\"nonsamplingErr\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}InternationalString\" minOccurs=\"0\"/>\r\n         &lt;element name=\"coherXDom\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}InternationalString\" minOccurs=\"0\"/>\r\n         &lt;element name=\"coherInternal\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}InternationalString\" minOccurs=\"0\"/>\r\n         &lt;element name=\"comment\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}InternationalString\" minOccurs=\"0\"/>\r\n       &lt;/sequence>\r\n       &lt;attribute name=\"kind\" use=\"required\" type=\"{http://www.w3.org/2001/XMLSchema}string\" />\r\n     &lt;/restriction>\r\n   &lt;/complexContent>\r\n &lt;/complexType>\r\n <\/pre>"
    }
    ,
    "xml_operations_InstanceTypes" : {
      "type" : "object",
      "title" : "InstanceTypes",
      "allOf" : [
        {
          "$ref" : "#/definitions/xml_cdomain_ListBase"
        },
        {
          "properties" : {
            "instanceType" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_Item"
            }
          }
        }
      ],
      "description" : "<p>Java class for InstanceTypes complex type.\r\n\r\n<p>The following schema fragment specifies the expected content contained within this class.\r\n\r\n<pre>\r\n &lt;complexType name=\"InstanceTypes\">\r\n   &lt;complexContent>\r\n     &lt;extension base=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}ListBase\">\r\n       &lt;sequence>\r\n         &lt;element name=\"instanceType\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}Item\" maxOccurs=\"unbounded\" minOccurs=\"0\"/>\r\n       &lt;/sequence>\r\n     &lt;/extension>\r\n   &lt;/complexContent>\r\n &lt;/complexType>\r\n <\/pre>"
    }
    ,
    "xml_operations_Instances" : {
      "type" : "object",
      "title" : "Instances",
      "allOf" : [
        {
          "$ref" : "#/definitions/xml_cdomain_ListBase"
        },
        {
          "properties" : {
            "instance" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_Resource"
            }
          }
        }
      ],
      "description" : "<p>Java class for Instances complex type.\r\n\r\n<p>The following schema fragment specifies the expected content contained within this class.\r\n\r\n<pre>\r\n &lt;complexType name=\"Instances\">\r\n   &lt;complexContent>\r\n     &lt;extension base=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}ListBase\">\r\n       &lt;sequence>\r\n         &lt;element name=\"instance\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}Resource\" maxOccurs=\"unbounded\" minOccurs=\"0\"/>\r\n       &lt;/sequence>\r\n     &lt;/extension>\r\n   &lt;/complexContent>\r\n &lt;/complexType>\r\n <\/pre>"
    }
    ,
    "xml_operations_LegalActs" : {
      "type" : "object",
      "title" : "LegalActs",
      "allOf" : [
        {
          "$ref" : "#/definitions/xml_cdomain_ListBase"
        },
        {
          "properties" : {
            "legalActs" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_InternationalString"
            }
          }
        }
      ],
      "description" : "<p>Java class for LegalActs complex type.\r\n\r\n<p>The following schema fragment specifies the expected content contained within this class.\r\n\r\n<pre>\r\n &lt;complexType name=\"LegalActs\">\r\n   &lt;complexContent>\r\n     &lt;extension base=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}ListBase\">\r\n       &lt;sequence>\r\n         &lt;element name=\"legalActs\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}InternationalString\" maxOccurs=\"unbounded\" minOccurs=\"0\"/>\r\n       &lt;/sequence>\r\n     &lt;/extension>\r\n   &lt;/complexContent>\r\n &lt;/complexType>\r\n <\/pre>"
    }
    ,
    "xml_operations_Measures" : {
      "type" : "object",
      "title" : "Measures",
      "allOf" : [
        {
          "$ref" : "#/definitions/xml_cdomain_ListBase"
        },
        {
          "properties" : {
            "measure" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_Resource"
            }
          }
        }
      ],
      "description" : "<p>Java class for Measures complex type.\r\n\r\n<p>The following schema fragment specifies the expected content contained within this class.\r\n\r\n<pre>\r\n &lt;complexType name=\"Measures\">\r\n   &lt;complexContent>\r\n     &lt;extension base=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}ListBase\">\r\n       &lt;sequence>\r\n         &lt;element name=\"measure\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}Resource\" maxOccurs=\"unbounded\" minOccurs=\"0\"/>\r\n       &lt;/sequence>\r\n     &lt;/extension>\r\n   &lt;/complexContent>\r\n &lt;/complexType>\r\n <\/pre>"
    }
    ,
    "xml_operations_OfficialityTypes" : {
      "type" : "object",
      "title" : "OfficialityTypes",
      "allOf" : [
        {
          "$ref" : "#/definitions/xml_cdomain_ListBase"
        },
        {
          "properties" : {
            "officialityType" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_Item"
            }
          }
        }
      ],
      "description" : "<p>Java class for OfficialityTypes complex type.\r\n\r\n<p>The following schema fragment specifies the expected content contained within this class.\r\n\r\n<pre>\r\n &lt;complexType name=\"OfficialityTypes\">\r\n   &lt;complexContent>\r\n     &lt;extension base=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}ListBase\">\r\n       &lt;sequence>\r\n         &lt;element name=\"officialityType\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}Item\" maxOccurs=\"unbounded\" minOccurs=\"0\"/>\r\n       &lt;/sequence>\r\n     &lt;/extension>\r\n   &lt;/complexContent>\r\n &lt;/complexType>\r\n <\/pre>"
    }
    ,
    "xml_operations_Operation" : {
      "type" : "object",
      "title" : "Operation",
      "allOf" : [
        {
          "properties" : {
            "kind" : {
              "xml" : {
                "attribute" : true,
                "namespace" : ""
              },
"description" : "",
"type" : "string"
            },
            "acronym" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_InternationalString"
            },
            "childLinks" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_ChildLinks"
            },
            "comment" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_InternationalString"
            },
            "confidentialityDataTreatment" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_InternationalString"
            },
            "confidentialityPolicy" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_InternationalString"
            },
            "contact" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_Resource"
            },
            "currentInstance" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_Resource"
            },
            "currentlyActive" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"type" : "boolean"
            },
            "dataSharings" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_operations_DataSharings"
            },
            "description" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_InternationalString"
            },
            "id" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"type" : "string"
            },
            "indicatorSystem" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"type" : "boolean"
            },
            "inventoryDate" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"type" : "string"
            },
            "legalActs" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_operations_LegalActs"
            },
            "name" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_InternationalString"
            },
            "objective" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_InternationalString"
            },
            "officialityType" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_Item"
            },
            "parentLink" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_ResourceLink"
            },
            "producers" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_operations_Producers"
            },
            "publishers" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_operations_Publishers"
            },
            "regionalContributors" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_operations_RegionalContributors"
            },
            "regionalResponsibles" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_operations_RegionalResponsibles"
            },
            "relPolUsAc" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_InternationalString"
            },
            "releaseCalendar" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"type" : "boolean"
            },
            "releaseCalendarAccess" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"type" : "string"
            },
            "revPolicy" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_InternationalString"
            },
            "revPractice" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_InternationalString"
            },
            "secondarySubjectAreas" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_operations_SecondarySubjectAreas"
            },
            "selfLink" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_ResourceLink"
            },
            "statisticalOperationType" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_Item"
            },
            "status" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_operations_Status"
            },
            "subjectArea" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_Resource"
            },
            "updateFrequencies" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_operations_UpdateFrequencies"
            },
            "urn" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"type" : "string"
            }
          }
        }
      ],
      "description" : "<p>Java class for Operation complex type.\r\n\r\n<p>The following schema fragment specifies the expected content contained within this class.\r\n\r\n<pre>\r\n &lt;complexType name=\"Operation\">\r\n   &lt;complexContent>\r\n     &lt;restriction base=\"{http://www.w3.org/2001/XMLSchema}anyType\">\r\n       &lt;sequence>\r\n         &lt;element name=\"id\" type=\"{http://www.w3.org/2001/XMLSchema}string\"/>\r\n         &lt;element name=\"urn\" type=\"{http://www.w3.org/2001/XMLSchema}string\"/>\r\n         &lt;element name=\"selfLink\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}ResourceLink\"/>\r\n         &lt;element name=\"parentLink\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}ResourceLink\"/>\r\n         &lt;element name=\"childLinks\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}ChildLinks\"/>\r\n         &lt;element name=\"name\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}InternationalString\"/>\r\n         &lt;element name=\"acronym\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}InternationalString\" minOccurs=\"0\"/>\r\n         &lt;element name=\"subjectArea\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}Resource\"/>\r\n         &lt;element name=\"secondarySubjectAreas\" type=\"{http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain}SecondarySubjectAreas\" minOccurs=\"0\"/>\r\n         &lt;element name=\"objective\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}InternationalString\"/>\r\n         &lt;element name=\"description\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}InternationalString\" minOccurs=\"0\"/>\r\n         &lt;element name=\"statisticalOperationType\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}Item\"/>\r\n         &lt;element name=\"officialityType\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}Item\"/>\r\n         &lt;element name=\"indicatorSystem\" type=\"{http://www.w3.org/2001/XMLSchema}boolean\" minOccurs=\"0\"/>\r\n         &lt;element name=\"producers\" type=\"{http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain}Producers\"/>\r\n         &lt;element name=\"regionalResponsibles\" type=\"{http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain}RegionalResponsibles\"/>\r\n         &lt;element name=\"regionalContributors\" type=\"{http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain}RegionalContributors\" minOccurs=\"0\"/>\r\n         &lt;element name=\"currentlyActive\" type=\"{http://www.w3.org/2001/XMLSchema}boolean\" minOccurs=\"0\"/>\r\n         &lt;element name=\"status\" type=\"{http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain}Status\"/>\r\n         &lt;element name=\"publishers\" type=\"{http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain}Publishers\"/>\r\n         &lt;element name=\"relPolUsAc\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}InternationalString\" minOccurs=\"0\"/>\r\n         &lt;element name=\"releaseCalendar\" type=\"{http://www.w3.org/2001/XMLSchema}boolean\" minOccurs=\"0\"/>\r\n         &lt;element name=\"releaseCalendarAccess\" type=\"{http://www.w3.org/2001/XMLSchema}string\" minOccurs=\"0\"/>\r\n         &lt;element name=\"updateFrequencies\" type=\"{http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain}UpdateFrequencies\" minOccurs=\"0\"/>\r\n         &lt;element name=\"currentInstance\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}Resource\" minOccurs=\"0\"/>\r\n         &lt;element name=\"inventoryDate\" type=\"{http://www.w3.org/2001/XMLSchema}dateTime\" minOccurs=\"0\"/>\r\n         &lt;element name=\"revPolicy\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}InternationalString\" minOccurs=\"0\"/>\r\n         &lt;element name=\"revPractice\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}InternationalString\" minOccurs=\"0\"/>\r\n         &lt;element name=\"contact\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}Resource\"/>\r\n         &lt;element name=\"legalActs\" type=\"{http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain}LegalActs\" minOccurs=\"0\"/>\r\n         &lt;element name=\"dataSharings\" type=\"{http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain}DataSharings\" minOccurs=\"0\"/>\r\n         &lt;element name=\"confidentialityPolicy\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}InternationalString\" minOccurs=\"0\"/>\r\n         &lt;element name=\"confidentialityDataTreatment\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}InternationalString\" minOccurs=\"0\"/>\r\n         &lt;element name=\"comment\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}InternationalString\" minOccurs=\"0\"/>\r\n       &lt;/sequence>\r\n       &lt;attribute name=\"kind\" use=\"required\" type=\"{http://www.w3.org/2001/XMLSchema}string\" />\r\n     &lt;/restriction>\r\n   &lt;/complexContent>\r\n &lt;/complexType>\r\n <\/pre>"
    }
    ,
    "xml_operations_Operations" : {
      "type" : "object",
      "title" : "Operations",
      "allOf" : [
        {
          "$ref" : "#/definitions/xml_cdomain_ListBase"
        },
        {
          "properties" : {
            "operation" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_Resource"
            }
          }
        }
      ],
      "description" : "<p>Java class for Operations complex type.\r\n\r\n<p>The following schema fragment specifies the expected content contained within this class.\r\n\r\n<pre>\r\n &lt;complexType name=\"Operations\">\r\n   &lt;complexContent>\r\n     &lt;extension base=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}ListBase\">\r\n       &lt;sequence>\r\n         &lt;element name=\"operation\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}Resource\" maxOccurs=\"unbounded\" minOccurs=\"0\"/>\r\n       &lt;/sequence>\r\n     &lt;/extension>\r\n   &lt;/complexContent>\r\n &lt;/complexType>\r\n <\/pre>"
    }
    ,
    "xml_operations_Producers" : {
      "type" : "object",
      "title" : "Producers",
      "allOf" : [
        {
          "$ref" : "#/definitions/xml_cdomain_ListBase"
        },
        {
          "properties" : {
            "producer" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_Resource"
            }
          }
        }
      ],
      "description" : "<p>Java class for Producers complex type.\r\n\r\n<p>The following schema fragment specifies the expected content contained within this class.\r\n\r\n<pre>\r\n &lt;complexType name=\"Producers\">\r\n   &lt;complexContent>\r\n     &lt;extension base=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}ListBase\">\r\n       &lt;sequence>\r\n         &lt;element name=\"producer\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}Resource\" maxOccurs=\"unbounded\" minOccurs=\"0\"/>\r\n       &lt;/sequence>\r\n     &lt;/extension>\r\n   &lt;/complexContent>\r\n &lt;/complexType>\r\n <\/pre>"
    }
    ,
    "xml_operations_Publishers" : {
      "type" : "object",
      "title" : "Publishers",
      "allOf" : [
        {
          "$ref" : "#/definitions/xml_cdomain_ListBase"
        },
        {
          "properties" : {
            "publisher" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_Resource"
            }
          }
        }
      ],
      "description" : "<p>Java class for Publishers complex type.\r\n\r\n<p>The following schema fragment specifies the expected content contained within this class.\r\n\r\n<pre>\r\n &lt;complexType name=\"Publishers\">\r\n   &lt;complexContent>\r\n     &lt;extension base=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}ListBase\">\r\n       &lt;sequence>\r\n         &lt;element name=\"publisher\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}Resource\" maxOccurs=\"unbounded\" minOccurs=\"0\"/>\r\n       &lt;/sequence>\r\n     &lt;/extension>\r\n   &lt;/complexContent>\r\n &lt;/complexType>\r\n <\/pre>"
    }
    ,
    "xml_operations_RegionalContributors" : {
      "type" : "object",
      "title" : "RegionalContributors",
      "allOf" : [
        {
          "$ref" : "#/definitions/xml_cdomain_ListBase"
        },
        {
          "properties" : {
            "regionalContributor" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_Resource"
            }
          }
        }
      ],
      "description" : "<p>Java class for RegionalContributors complex type.\r\n\r\n<p>The following schema fragment specifies the expected content contained within this class.\r\n\r\n<pre>\r\n &lt;complexType name=\"RegionalContributors\">\r\n   &lt;complexContent>\r\n     &lt;extension base=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}ListBase\">\r\n       &lt;sequence>\r\n         &lt;element name=\"regionalContributor\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}Resource\" maxOccurs=\"unbounded\" minOccurs=\"0\"/>\r\n       &lt;/sequence>\r\n     &lt;/extension>\r\n   &lt;/complexContent>\r\n &lt;/complexType>\r\n <\/pre>"
    }
    ,
    "xml_operations_RegionalResponsibles" : {
      "type" : "object",
      "title" : "RegionalResponsibles",
      "allOf" : [
        {
          "$ref" : "#/definitions/xml_cdomain_ListBase"
        },
        {
          "properties" : {
            "regionalResponsible" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_Resource"
            }
          }
        }
      ],
      "description" : "<p>Java class for RegionalResponsibles complex type.\r\n\r\n<p>The following schema fragment specifies the expected content contained within this class.\r\n\r\n<pre>\r\n &lt;complexType name=\"RegionalResponsibles\">\r\n   &lt;complexContent>\r\n     &lt;extension base=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}ListBase\">\r\n       &lt;sequence>\r\n         &lt;element name=\"regionalResponsible\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}Resource\" maxOccurs=\"unbounded\" minOccurs=\"0\"/>\r\n       &lt;/sequence>\r\n     &lt;/extension>\r\n   &lt;/complexContent>\r\n &lt;/complexType>\r\n <\/pre>"
    }
    ,
    "xml_operations_SecondarySubjectAreas" : {
      "type" : "object",
      "title" : "SecondarySubjectAreas",
      "allOf" : [
        {
          "$ref" : "#/definitions/xml_cdomain_ListBase"
        },
        {
          "properties" : {
            "secondarySubjectArea" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_Resource"
            }
          }
        }
      ],
      "description" : "<p>Java class for SecondarySubjectAreas complex type.\r\n\r\n<p>The following schema fragment specifies the expected content contained within this class.\r\n\r\n<pre>\r\n &lt;complexType name=\"SecondarySubjectAreas\">\r\n   &lt;complexContent>\r\n     &lt;extension base=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}ListBase\">\r\n       &lt;sequence>\r\n         &lt;element name=\"secondarySubjectArea\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}Resource\" maxOccurs=\"unbounded\" minOccurs=\"0\"/>\r\n       &lt;/sequence>\r\n     &lt;/extension>\r\n   &lt;/complexContent>\r\n &lt;/complexType>\r\n <\/pre>"
    }
    ,
    "xml_operations_StatConcDefs" : {
      "type" : "object",
      "title" : "StatConcDefs",
      "allOf" : [
        {
          "$ref" : "#/definitions/xml_cdomain_ListBase"
        },
        {
          "properties" : {
            "statConcDef" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_Resource"
            }
          }
        }
      ],
      "description" : "<p>Java class for StatConcDefs complex type.\r\n\r\n<p>The following schema fragment specifies the expected content contained within this class.\r\n\r\n<pre>\r\n &lt;complexType name=\"StatConcDefs\">\r\n   &lt;complexContent>\r\n     &lt;extension base=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}ListBase\">\r\n       &lt;sequence>\r\n         &lt;element name=\"statConcDef\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}Resource\" maxOccurs=\"unbounded\" minOccurs=\"0\"/>\r\n       &lt;/sequence>\r\n     &lt;/extension>\r\n   &lt;/complexContent>\r\n &lt;/complexType>\r\n <\/pre>"
    }
    ,
    "xml_operations_StatisticalOperationSources" : {
      "type" : "object",
      "title" : "StatisticalOperationSources",
      "allOf" : [
        {
          "$ref" : "#/definitions/xml_cdomain_ListBase"
        },
        {
          "properties" : {
            "statisticalOperationSource" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_Item"
            }
          }
        }
      ],
      "description" : "<p>Java class for StatisticalOperationSources complex type.\r\n\r\n<p>The following schema fragment specifies the expected content contained within this class.\r\n\r\n<pre>\r\n &lt;complexType name=\"StatisticalOperationSources\">\r\n   &lt;complexContent>\r\n     &lt;extension base=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}ListBase\">\r\n       &lt;sequence>\r\n         &lt;element name=\"statisticalOperationSource\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}Item\" maxOccurs=\"unbounded\" minOccurs=\"0\"/>\r\n       &lt;/sequence>\r\n     &lt;/extension>\r\n   &lt;/complexContent>\r\n &lt;/complexType>\r\n <\/pre>"
    }
    ,
    "xml_operations_StatisticalOperationTypes" : {
      "type" : "object",
      "title" : "StatisticalOperationTypes",
      "allOf" : [
        {
          "$ref" : "#/definitions/xml_cdomain_ListBase"
        },
        {
          "properties" : {
            "statisticalOperationType" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_Item"
            }
          }
        }
      ],
      "description" : "<p>Java class for StatisticalOperationTypes complex type.\r\n\r\n<p>The following schema fragment specifies the expected content contained within this class.\r\n\r\n<pre>\r\n &lt;complexType name=\"StatisticalOperationTypes\">\r\n   &lt;complexContent>\r\n     &lt;extension base=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}ListBase\">\r\n       &lt;sequence>\r\n         &lt;element name=\"statisticalOperationType\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}Item\" maxOccurs=\"unbounded\" minOccurs=\"0\"/>\r\n       &lt;/sequence>\r\n     &lt;/extension>\r\n   &lt;/complexContent>\r\n &lt;/complexType>\r\n <\/pre>"
    }
    ,
    "xml_operations_StatisticalUnits" : {
      "type" : "object",
      "title" : "StatisticalUnits",
      "allOf" : [
        {
          "$ref" : "#/definitions/xml_cdomain_ListBase"
        },
        {
          "properties" : {
            "statisticalUnit" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_Resource"
            }
          }
        }
      ],
      "description" : "<p>Java class for StatisticalUnits complex type.\r\n\r\n<p>The following schema fragment specifies the expected content contained within this class.\r\n\r\n<pre>\r\n &lt;complexType name=\"StatisticalUnits\">\r\n   &lt;complexContent>\r\n     &lt;extension base=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}ListBase\">\r\n       &lt;sequence>\r\n         &lt;element name=\"statisticalUnit\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}Resource\" maxOccurs=\"unbounded\" minOccurs=\"0\"/>\r\n       &lt;/sequence>\r\n     &lt;/extension>\r\n   &lt;/complexContent>\r\n &lt;/complexType>\r\n <\/pre>"
    }
    ,
    "xml_operations_Status" : {
      "type" : "string",
      "title" : "Status",
          "enum" : [
            "PLANNING",
            "DESIGN",
            "PRODUCTION",
            "OUT_OF_PRINT"
          ],
      "description" : "<p>Java class for Status.\r\n\r\n<p>The following schema fragment specifies the expected content contained within this class.\r\n<p>\r\n<pre>\r\n &lt;simpleType name=\"Status\">\r\n   &lt;restriction base=\"{http://www.w3.org/2001/XMLSchema}token\">\r\n     &lt;enumeration value=\"PLANNING\"/>\r\n     &lt;enumeration value=\"DESIGN\"/>\r\n     &lt;enumeration value=\"PRODUCTION\"/>\r\n     &lt;enumeration value=\"OUT_OF_PRINT\"/>\r\n   &lt;/restriction>\r\n &lt;/simpleType>\r\n <\/pre>"
    }
    ,
    "xml_operations_TemporalGranularities" : {
      "type" : "object",
      "title" : "TemporalGranularities",
      "allOf" : [
        {
          "$ref" : "#/definitions/xml_cdomain_ListBase"
        },
        {
          "properties" : {
            "temporalGranularity" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_Resource"
            }
          }
        }
      ],
      "description" : "<p>Java class for TemporalGranularities complex type.\r\n\r\n<p>The following schema fragment specifies the expected content contained within this class.\r\n\r\n<pre>\r\n &lt;complexType name=\"TemporalGranularities\">\r\n   &lt;complexContent>\r\n     &lt;extension base=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}ListBase\">\r\n       &lt;sequence>\r\n         &lt;element name=\"temporalGranularity\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}Resource\" maxOccurs=\"unbounded\" minOccurs=\"0\"/>\r\n       &lt;/sequence>\r\n     &lt;/extension>\r\n   &lt;/complexContent>\r\n &lt;/complexType>\r\n <\/pre>"
    }
    ,
    "xml_operations_UpdateFrequencies" : {
      "type" : "object",
      "title" : "UpdateFrequencies",
      "allOf" : [
        {
          "$ref" : "#/definitions/xml_cdomain_ListBase"
        },
        {
          "properties" : {
            "updateFrequency" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/statistical-operations/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_Resource"
            }
          }
        }
      ],
      "description" : "<p>Java class for UpdateFrequencies complex type.\r\n\r\n<p>The following schema fragment specifies the expected content contained within this class.\r\n\r\n<pre>\r\n &lt;complexType name=\"UpdateFrequencies\">\r\n   &lt;complexContent>\r\n     &lt;extension base=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}ListBase\">\r\n       &lt;sequence>\r\n         &lt;element name=\"updateFrequency\" type=\"{http://www.siemac.org/metamac/rest/common/v1.0/domain}Resource\" maxOccurs=\"unbounded\" minOccurs=\"0\"/>\r\n       &lt;/sequence>\r\n     &lt;/extension>\r\n   &lt;/complexContent>\r\n &lt;/complexType>\r\n <\/pre>"
    }
    ,
    "xml_cdomain_ChildLinks" : {
      "type" : "object",
      "title" : "ChildLinks",
      "allOf" : [
        {
          "properties" : {
            "total" : {
              "xml" : {
                "attribute" : true,
                "namespace" : ""
              },
"description" : "",
"type" : "number"
            },
            "childLink" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/common/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_ResourceLink"
            }
          }
        }
      ],
      "description" : ""
    }
    ,
    "xml_cdomain_InternationalString" : {
      "type" : "object",
      "title" : "InternationalString",
      "allOf" : [
        {
          "properties" : {
            "text" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/common/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_LocalisedString"
            }
          }
        }
      ],
      "description" : ""
    }
    ,
    "xml_cdomain_Item" : {
      "type" : "object",
      "title" : "Item",
      "allOf" : [
        {
          "properties" : {
            "id" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/common/v1.0/domain"
              },
"description" : "",
"type" : "string"
            },
            "name" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/common/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_InternationalString"
            }
          }
        }
      ],
      "description" : ""
    }
    ,
    "xml_cdomain_ListBase" : {
      "type" : "object",
      "title" : "ListBase",
      "allOf" : [
        {
          "properties" : {
            "firstLink" : {
              "xml" : {
                "attribute" : true,
                "namespace" : ""
              },
"description" : "",
"type" : "string"
            },
            "kind" : {
              "xml" : {
                "attribute" : true,
                "namespace" : ""
              },
"description" : "",
"type" : "string"
            },
            "lastLink" : {
              "xml" : {
                "attribute" : true,
                "namespace" : ""
              },
"description" : "",
"type" : "string"
            },
            "limit" : {
              "xml" : {
                "attribute" : true,
                "namespace" : ""
              },
"description" : "",
"type" : "number"
            },
            "nextLink" : {
              "xml" : {
                "attribute" : true,
                "namespace" : ""
              },
"description" : "",
"type" : "string"
            },
            "offset" : {
              "xml" : {
                "attribute" : true,
                "namespace" : ""
              },
"description" : "",
"type" : "number"
            },
            "previousLink" : {
              "xml" : {
                "attribute" : true,
                "namespace" : ""
              },
"description" : "",
"type" : "string"
            },
            "selfLink" : {
              "xml" : {
                "attribute" : true,
                "namespace" : ""
              },
"description" : "",
"type" : "string"
            },
            "total" : {
              "xml" : {
                "attribute" : true,
                "namespace" : ""
              },
"description" : "",
"type" : "number"
            }
          }
        }
      ],
      "description" : ""
    }
    ,
    "xml_cdomain_LocalisedString" : {
      "type" : "object",
      "title" : "LocalisedString",
      "allOf" : [
        {
          "properties" : {
            "lang" : {
              "xml" : {
                "attribute" : true,
                "namespace" : "http://www.w3.org/XML/1998/namespace"
              },
"description" : "",
"type" : "string"
            },
            "(value)" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/common/v1.0/domain"
              },
"description" : "",
"type" : "string"
            }
          }
        }
      ],
      "description" : ""
    }
    ,
    "xml_cdomain_Resource" : {
      "type" : "object",
      "title" : "Resource",
      "allOf" : [
        {
          "properties" : {
            "kind" : {
              "xml" : {
                "attribute" : true,
                "namespace" : ""
              },
"description" : "",
"type" : "string"
            },
            "id" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/common/v1.0/domain"
              },
"description" : "",
"type" : "string"
            },
            "name" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/common/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_InternationalString"
            },
            "nestedId" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/common/v1.0/domain"
              },
"description" : "",
"type" : "string"
            },
            "selfLink" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/common/v1.0/domain"
              },
"description" : "",
"$ref" : "#/definitions/xml_cdomain_ResourceLink"
            },
            "urn" : {
              "xml" : {
                "namespace" : "http://www.siemac.org/metamac/rest/common/v1.0/domain"
              },
"description" : "",
"type" : "string"
            }
          }
        }
      ],
      "description" : ""
    }
    ,
    "xml_cdomain_ResourceLink" : {
      "type" : "object",
      "title" : "ResourceLink",
      "allOf" : [
        {
          "properties" : {
            "href" : {
              "xml" : {
                "attribute" : true,
                "namespace" : ""
              },
"description" : "",
"type" : "string"
            },
            "kind" : {
              "xml" : {
                "attribute" : true,
                "namespace" : ""
              },
"description" : "",
"type" : "string"
            }
          }
        }
      ],
      "description" : ""
    }
  },
  "paths": {
    "/v1.0/statisticalOperationSources" : {
      "get" : {
        "tags" : [ "StatisticalOperationsV1_0" ],
        "description" : "Retrieve all statistical operations sources",
        "operationId" : "resource_StatisticalOperationsV1_0_retrieveStatisticalOperationSources_GET",
        "produces" : [ "application/xml" ],
        "parameters" : [
        ],
        "responses" : {
          "200" : {
            "schema" : {
"description" : "",
"$ref" : "#/definitions/xml_operations_StatisticalOperationSources"
            },
            "headers" : {
            },
            "description" : "Success"
          },
          "default" : {
            "description" : "Unexpected error."
          }
        }
      }
    }
    ,
    "/v1.0/operations" : {
      "get" : {
        "tags" : [ "StatisticalOperationsV1_0" ],
        "description" : "Find operations",
        "operationId" : "resource_StatisticalOperationsV1_0_findOperations_GET",
        "produces" : [ "application/xml" ],
        "parameters" : [
          {
            "name" : "limit",
            "in" : "query",
            "type" : "string",
            "description" : "Maximum number of results per page"
          },
          {
            "name" : "offset",
            "in" : "query",
            "type" : "string",
            "description" : "Position of first result"
          },
          {
            "name" : "orderBy",
            "in" : "query",
            "type" : "string",
            "description" : "Clause to order the results by metadata <br/>\r\n- Order operators: ASC, DESC<br/>\r\n- Metadata to order: ID<br/>\r\n- Example: ID ASC<br/>"
          },
          {
            "name" : "query",
            "in" : "query",
            "type" : "string",
            "description" : "Clause to filter results by metadata <br/>\r\n- Logical operators: AND, OR <br/>\r\n- Comparison operators: EQ, IEQ, LIKE, ILIKE, NE, LT, LE, GT, GE, IS_NULL, IS_NOT_NULL, IN <br/>\r\n- Metadata to filter: ID, URN, TITLE, ACRONYM, SUBJECT_AREA_URN, SECONDARY_SUBJECT_AREA_URN, DESCRIPTION, StatisticalOperation_TYPE_ID, OFFICIALITY_TYPE_ID, IS_INDICATORS_SYSTEM,\r\nPRODUCER_URN,\r\nCURRENTLY_ACTIVE, STATUS, PROC_STATUS, PUBLISHER_URN, INVENTORY_DATE <br/>\r\n- Example: (ID LIKE \"Operation1\" AND OFFICIALITY_TYPE_ID EQ \"OfficialityType1\") OR (ACRONYM EQ \"Op1\") OR (INVENTORY_DATE IS_NULL)"
          }
        ],
        "responses" : {
          "200" : {
            "schema" : {
"description" : "",
"$ref" : "#/definitions/xml_operations_Operations"
            },
            "headers" : {
            },
            "description" : "Success"
          },
          "default" : {
            "description" : "Unexpected error."
          }
        }
      }
    }
    ,
    "/v1.0/operations/{id}/families" : {
      "get" : {
        "tags" : [ "StatisticalOperationsV1_0" ],
        "description" : "Retrieve families by operation",
        "operationId" : "resource_StatisticalOperationsV1_0_retrieveFamiliesByOperation_GET",
        "produces" : [ "application/xml" ],
        "parameters" : [
          {
            "name" : "id",
            "in" : "path",
            "type" : "string",
            "description" : "Id operation"
          }
        ],
        "responses" : {
          "200" : {
            "schema" : {
"description" : "",
"$ref" : "#/definitions/xml_operations_Families"
            },
            "headers" : {
            },
            "description" : "Success"
          },
          "default" : {
            "description" : "Unexpected error."
          }
        }
      }
    }
    ,
    "/v1.0/operations/{operationId}/instances/{id}" : {
      "get" : {
        "tags" : [ "StatisticalOperationsV1_0" ],
        "description" : "Retrieve instance by id",
        "operationId" : "resource_StatisticalOperationsV1_0_retrieveInstanceById_GET",
        "produces" : [ "application/xml" ],
        "parameters" : [
          {
            "name" : "id",
            "in" : "path",
            "type" : "string",
            "description" : "Instance id"
          },
          {
            "name" : "operationId",
            "in" : "path",
            "type" : "string",
            "description" : "Operation id"
          }
        ],
        "responses" : {
          "200" : {
            "schema" : {
"description" : "",
"$ref" : "#/definitions/xml_operations_Instance"
            },
            "headers" : {
            },
            "description" : "Success"
          },
          "default" : {
            "description" : "Unexpected error."
          }
        }
      }
    }
    ,
    "/v1.0/officialityTypes" : {
      "get" : {
        "tags" : [ "StatisticalOperationsV1_0" ],
        "description" : "Retrieve all officiality types",
        "operationId" : "resource_StatisticalOperationsV1_0_retrieveOfficialityTypes_GET",
        "produces" : [ "application/xml" ],
        "parameters" : [
        ],
        "responses" : {
          "200" : {
            "schema" : {
"description" : "",
"$ref" : "#/definitions/xml_operations_OfficialityTypes"
            },
            "headers" : {
            },
            "description" : "Success"
          },
          "default" : {
            "description" : "Unexpected error."
          }
        }
      }
    }
    ,
    "/v1.0/operations/{operationId}/instances" : {
      "get" : {
        "tags" : [ "StatisticalOperationsV1_0" ],
        "description" : "Find instances by operation",
        "operationId" : "resource_StatisticalOperationsV1_0_findInstances_GET",
        "produces" : [ "application/xml" ],
        "parameters" : [
          {
            "name" : "operationId",
            "in" : "path",
            "type" : "string",
            "description" : "Operation id"
          },
          {
            "name" : "limit",
            "in" : "query",
            "type" : "string",
            "description" : "Maximum number of results per page"
          },
          {
            "name" : "offset",
            "in" : "query",
            "type" : "string",
            "description" : "Position of first result"
          },
          {
            "name" : "orderBy",
            "in" : "query",
            "type" : "string",
            "description" : "Clause to order the results by metadata <br/>\r\n- Order operators: ASC, DESC<br/>\r\n- Metadata to order: ID<br/>\r\n- Example: ID ASC<br/>"
          },
          {
            "name" : "query",
            "in" : "query",
            "type" : "string",
            "description" : "Clause to filter results by metadata <br/>\r\n- Logical operators: AND, OR <br/>\r\n- Comparison operators: EQ, IEQ, LIKE, ILIKE, NE, LT, LE, GT, GE, IS_NULL, IS_NOT_NULL, IN <br/>\r\n- Metadata to filter: ID, URN, TITLE, ACRONYM, DATA_DESCRIPTION, GEOGRAPHIC_GRANULARITY_URN, TEMPORAL_GRANULARITY_URN, INVENTORY_DATE <br/>\r\n- Example: (ID LIKE \"Instance1\" AND DATA_DESCRIPTION EQ \"DataDescription1\") OR (ACRONYM EQ \"Instance1\")"
          }
        ],
        "responses" : {
          "200" : {
            "schema" : {
"description" : "",
"$ref" : "#/definitions/xml_operations_Instances"
            },
            "headers" : {
            },
            "description" : "Success"
          },
          "default" : {
            "description" : "Unexpected error."
          }
        }
      }
    }
    ,
    "/v1.0/families" : {
      "get" : {
        "tags" : [ "StatisticalOperationsV1_0" ],
        "description" : "Find families",
        "operationId" : "resource_StatisticalOperationsV1_0_findFamilies_GET",
        "produces" : [ "application/xml" ],
        "parameters" : [
          {
            "name" : "limit",
            "in" : "query",
            "type" : "string",
            "description" : "Maximum number of results per page"
          },
          {
            "name" : "offset",
            "in" : "query",
            "type" : "string",
            "description" : "Position of first result"
          },
          {
            "name" : "orderBy",
            "in" : "query",
            "type" : "string",
            "description" : "Clause to order the results by metadata <br/>\r\n- Order operators: ASC, DESC<br/>\r\n- Metadata to order: ID<br/>\r\n- Example: ID ASC<br/>"
          },
          {
            "name" : "query",
            "in" : "query",
            "type" : "string",
            "description" : "Clause to filter results by metadata <br/>\r\n- Logical operators: AND, OR <br/>\r\n- Comparison operators: EQ, IEQ, LIKE, ILIKE, NE, LT, LE, GT, GE, IS_NULL, IS_NOT_NULL, IN <br/>\r\n- Metadata to filter: ID, URN, TITLE, ACRONYM, DESCRIPTION, INVENTORY_DATE <br/>\r\n- Example: (ID LIKE \"Family1\" AND ACRONYM EQ \"Family1\") OR (DESCRIPTION IS_NOT_NULL)"
          }
        ],
        "responses" : {
          "200" : {
            "schema" : {
"description" : "",
"$ref" : "#/definitions/xml_operations_Families"
            },
            "headers" : {
            },
            "description" : "Success"
          },
          "default" : {
            "description" : "Unexpected error."
          }
        }
      }
    }
    ,
    "/v1.0/costs" : {
      "get" : {
        "tags" : [ "StatisticalOperationsV1_0" ],
        "description" : "Retrieve all costs",
        "operationId" : "resource_StatisticalOperationsV1_0_retrieveCosts_GET",
        "produces" : [ "application/xml" ],
        "parameters" : [
        ],
        "responses" : {
          "200" : {
            "schema" : {
"description" : "",
"$ref" : "#/definitions/xml_operations_Costs"
            },
            "headers" : {
            },
            "description" : "Success"
          },
          "default" : {
            "description" : "Unexpected error."
          }
        }
      }
    }
    ,
    "/v1.0/families/{id}/operations" : {
      "get" : {
        "tags" : [ "StatisticalOperationsV1_0" ],
        "description" : "Find operations by family",
        "operationId" : "resource_StatisticalOperationsV1_0_findOperationsByFamily_GET",
        "produces" : [ "application/xml" ],
        "parameters" : [
          {
            "name" : "id",
            "in" : "path",
            "type" : "string",
            "description" : "Family id"
          },
          {
            "name" : "limit",
            "in" : "query",
            "type" : "string",
            "description" : "Maximum number of results per page"
          },
          {
            "name" : "offset",
            "in" : "query",
            "type" : "string",
            "description" : "Position of first result"
          },
          {
            "name" : "orderBy",
            "in" : "query",
            "type" : "string",
            "description" : "Clause to order the results by metadata <br/>\r\n- Order operators: ASC, DESC<br/>\r\n- Metadata to order: ID<br/>\r\n- Example: ID ASC<br/>"
          },
          {
            "name" : "query",
            "in" : "query",
            "type" : "string",
            "description" : "Clause to filter results by metadata <br/>\r\n- Logical operators: AND, OR <br/>\r\n- Comparison operators: EQ, IEQ, LIKE, ILIKE, NE, LT, LE, GT, GE, IS_NULL, IS_NOT_NULL, IN <br/>\r\n- Metadata to filter: ID, URN, TITLE, ACRONYM, SUBJECT_AREA_URN, SECONDARY_SUBJECT_AREA_URN, DESCRIPTION, StatisticalOperation_TYPE_ID, OFFICIALITY_TYPE_ID, IS_INDICATORS_SYSTEM,\r\nPRODUCER_URN,\r\nCURRENTLY_ACTIVE, STATUS, PROC_STATUS, PUBLISHER_URN, INVENTORY_DATE <br/>\r\n- Example: (ID LIKE \"Operation1\" AND OFFICIALITY_TYPE_ID EQ \"OfficialityType1\") OR (ACRONYM EQ \"Op1\") OR (INVENTORY_DATE IS_NULL)"
          }
        ],
        "responses" : {
          "200" : {
            "schema" : {
"description" : "",
"$ref" : "#/definitions/xml_operations_Operations"
            },
            "headers" : {
            },
            "description" : "Success"
          },
          "default" : {
            "description" : "Unexpected error."
          }
        }
      }
    }
    ,
    "/v1.0/operations/{id}" : {
      "get" : {
        "tags" : [ "StatisticalOperationsV1_0" ],
        "description" : "Retrieve operation by id",
        "operationId" : "resource_StatisticalOperationsV1_0_retrieveOperationById_GET",
        "produces" : [ "application/xml" ],
        "parameters" : [
          {
            "name" : "id",
            "in" : "path",
            "type" : "string",
            "description" : "Id"
          }
        ],
        "responses" : {
          "200" : {
            "schema" : {
"description" : "",
"$ref" : "#/definitions/xml_operations_Operation"
            },
            "headers" : {
            },
            "description" : "Success"
          },
          "default" : {
            "description" : "Unexpected error."
          }
        }
      }
    }
    ,
    "/v1.0/collMethods" : {
      "get" : {
        "tags" : [ "StatisticalOperationsV1_0" ],
        "description" : "Retrieve all coll methods",
        "operationId" : "resource_StatisticalOperationsV1_0_retrieveCollMethods_GET",
        "produces" : [ "application/xml" ],
        "parameters" : [
        ],
        "responses" : {
          "200" : {
            "schema" : {
"description" : "",
"$ref" : "#/definitions/xml_operations_CollMethods"
            },
            "headers" : {
            },
            "description" : "Success"
          },
          "default" : {
            "description" : "Unexpected error."
          }
        }
      }
    }
    ,
    "/v1.0/instanceTypes" : {
      "get" : {
        "tags" : [ "StatisticalOperationsV1_0" ],
        "description" : "Retrieve all instance types",
        "operationId" : "resource_StatisticalOperationsV1_0_retrieveInstanceTypes_GET",
        "produces" : [ "application/xml" ],
        "parameters" : [
        ],
        "responses" : {
          "200" : {
            "schema" : {
"description" : "",
"$ref" : "#/definitions/xml_operations_InstanceTypes"
            },
            "headers" : {
            },
            "description" : "Success"
          },
          "default" : {
            "description" : "Unexpected error."
          }
        }
      }
    }
    ,
    "/v1.0/families/{id}" : {
      "get" : {
        "tags" : [ "StatisticalOperationsV1_0" ],
        "description" : "Retrieve family by id",
        "operationId" : "resource_StatisticalOperationsV1_0_retrieveFamilyById_GET",
        "produces" : [ "application/xml" ],
        "parameters" : [
          {
            "name" : "id",
            "in" : "path",
            "type" : "string",
            "description" : "Family id"
          }
        ],
        "responses" : {
          "200" : {
            "schema" : {
"description" : "",
"$ref" : "#/definitions/xml_operations_Family"
            },
            "headers" : {
            },
            "description" : "Success"
          },
          "default" : {
            "description" : "Unexpected error."
          }
        }
      }
    }
    ,
    "/v1.0/statisticalOperationTypes" : {
      "get" : {
        "tags" : [ "StatisticalOperationsV1_0" ],
        "description" : "Retrieve all statistical operations types",
        "operationId" : "resource_StatisticalOperationsV1_0_retrieveStatisticalOperationTypes_GET",
        "produces" : [ "application/xml" ],
        "parameters" : [
        ],
        "responses" : {
          "200" : {
            "schema" : {
"description" : "",
"$ref" : "#/definitions/xml_operations_StatisticalOperationTypes"
            },
            "headers" : {
            },
            "description" : "Success"
          },
          "default" : {
            "description" : "Unexpected error."
          }
        }
      }
    }
  }
}
