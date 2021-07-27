package org.siemac.metamac.statistical.operations.core.stream.mappers.impl;

import org.siemac.metamac.statistical.operations.core.domain.Cost;
import org.siemac.metamac.statistical.operations.core.stream.mappers.Do2AvroMapper;
import org.siemac.metamac.statistical.operations.core.stream.messages.CostAvro;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class CostDo2AvroMapper implements Do2AvroMapper<Cost, CostAvro> {
    @Autowired
    InternationalStringDo2AvroMapper internationalStringDo2AvroMapper;

    @Override
    public CostAvro toAvro(Cost source) {
        if (source == null) {
            return null;
        }
        return CostAvro.newBuilder()
                       .setIdentifier(source.getIdentifier())
                       .setDescription(internationalStringDo2AvroMapper.toAvro(source.getDescription()))
                       .build();
    }
}
