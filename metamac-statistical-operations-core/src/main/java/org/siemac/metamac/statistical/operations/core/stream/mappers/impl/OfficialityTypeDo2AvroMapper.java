package org.siemac.metamac.statistical.operations.core.stream.mappers.impl;

import org.siemac.metamac.statistical.operations.core.domain.OfficialityType;
import org.siemac.metamac.statistical.operations.core.stream.mappers.Do2AvroMapper;
import org.siemac.metamac.statistical.operations.core.stream.messages.OfficialityTypeAvro;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class OfficialityTypeDo2AvroMapper implements Do2AvroMapper<OfficialityType, OfficialityTypeAvro> {
    @Autowired
    InternationalStringDo2AvroMapper internationalStringDo2AvroMapper;

    @Override
    public OfficialityTypeAvro toAvro(OfficialityType source) {
        if (source == null) {
            return null;
        }
        return OfficialityTypeAvro.newBuilder()
                                  .setIdentifier(source.getIdentifier())
                                  .setDescription(internationalStringDo2AvroMapper.toAvro(source.getDescription()))
                                  .build();
    }
}
