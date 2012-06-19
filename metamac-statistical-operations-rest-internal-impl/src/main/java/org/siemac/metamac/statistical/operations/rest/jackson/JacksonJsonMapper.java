package org.siemac.metamac.statistical.operations.rest.jackson;

import org.codehaus.jackson.map.ObjectMapper;
import org.codehaus.jackson.map.SerializationConfig;
import org.codehaus.jackson.map.annotate.JsonSerialize;

// TODO pasar a librería común
public class JacksonJsonMapper extends ObjectMapper {

    public JacksonJsonMapper() {
        // SimpleModule module = new SimpleModule("NAME", new Version(1, 0, 0, null));
        // module.addSerializer(DateTime.class, new DateTimeSerializer());
        // objectMapper.registerModule(module);
        this.setSerializationInclusion(JsonSerialize.Inclusion.NON_NULL);
        this.configure(SerializationConfig.Feature.INDENT_OUTPUT, true);
        this.configure(SerializationConfig.Feature.WRITE_EMPTY_JSON_ARRAYS, false);
    }
}
