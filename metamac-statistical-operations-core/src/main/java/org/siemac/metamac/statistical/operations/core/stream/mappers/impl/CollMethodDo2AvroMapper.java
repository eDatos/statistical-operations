package org.siemac.metamac.statistical.operations.core.stream.mappers.impl;

import org.siemac.metamac.statistical.operations.core.domain.CollMethod;
import org.siemac.metamac.statistical.operations.core.stream.mappers.Do2AvroMapper;
import org.siemac.metamac.statistical.operations.core.stream.messages.CollMethodAvro;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class CollMethodDo2AvroMapper implements Do2AvroMapper<CollMethod, CollMethodAvro> {
    @Autowired
    InternationalStringDo2AvroMapper internationalStringDo2AvroMapper;

    @Override
    public CollMethodAvro toAvro(CollMethod source) {
        if (source == null) {
            return null;
        }
        return CollMethodAvro.newBuilder()
                             .setIdentifier(source.getIdentifier())
                             .setDescription(internationalStringDo2AvroMapper.toAvro(source.getDescription()))
                             .build();
    }
}
