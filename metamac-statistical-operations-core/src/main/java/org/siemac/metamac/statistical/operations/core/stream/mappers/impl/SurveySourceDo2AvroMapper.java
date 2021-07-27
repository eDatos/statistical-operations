package org.siemac.metamac.statistical.operations.core.stream.mappers.impl;

import org.siemac.metamac.statistical.operations.core.domain.SurveySource;
import org.siemac.metamac.statistical.operations.core.stream.mappers.Do2AvroMapper;
import org.siemac.metamac.statistical.operations.core.stream.messages.SurveySourceAvro;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class SurveySourceDo2AvroMapper implements Do2AvroMapper<SurveySource, SurveySourceAvro> {
    @Autowired
    InternationalStringDo2AvroMapper internationalStringDo2AvroMapper;

    @Override
    public SurveySourceAvro toAvro(SurveySource source) {
        if (source == null) {
            return null;
        }
        return SurveySourceAvro.newBuilder()
                               .setIdentifier(source.getIdentifier())
                               .setDescription(internationalStringDo2AvroMapper.toAvro(source.getDescription()))
                               .build();
    }
}
