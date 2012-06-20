package org.siemac.metamac.statistical.operations.rest.jackson;

import org.codehaus.jackson.map.ObjectMapper;
import org.codehaus.jackson.map.SerializationConfig;
import org.codehaus.jackson.map.annotate.JsonSerialize;

// TODO pasar a librería común
public class JacksonJsonMapper extends ObjectMapper {

    public JacksonJsonMapper() {
        this.setSerializationInclusion(JsonSerialize.Inclusion.NON_NULL);
//        this.configure(SerializationConfig.Feature.INDENT_OUTPUT, true); // do not indent, to consume less bytes
        this.configure(SerializationConfig.Feature.WRITE_EMPTY_JSON_ARRAYS, false);
        this.configure(SerializationConfig.Feature.WRITE_DATES_AS_TIMESTAMPS, false);
    }
}
