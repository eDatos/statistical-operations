package org.siemac.metamac.statistical.operations.core.stream.mappers.impl;

import org.siemac.metamac.statistical.operations.core.domain.SurveyType;
import org.siemac.metamac.statistical.operations.core.stream.mappers.Do2AvroMapper;
import org.siemac.metamac.statistical.operations.core.stream.messages.SurveyTypeAvro;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class SurveyTypeDo2AvroMapper implements Do2AvroMapper<SurveyType, SurveyTypeAvro> {
    @Autowired
    InternationalStringDo2AvroMapper internationalStringDo2AvroMapper;

    @Override
    public SurveyTypeAvro toAvro(SurveyType source) {
        if (source == null) {
            return null;
        }
        return SurveyTypeAvro.newBuilder()
                             .setIdentifier(source.getIdentifier())
                             .setDescription(internationalStringDo2AvroMapper.toAvro(source.getDescription()))
                             .build();
    }
}
