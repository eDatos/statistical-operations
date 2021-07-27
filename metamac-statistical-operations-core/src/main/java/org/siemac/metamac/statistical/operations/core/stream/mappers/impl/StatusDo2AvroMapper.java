package org.siemac.metamac.statistical.operations.core.stream.mappers.impl;

import org.siemac.metamac.statistical.operations.core.enume.domain.StatusEnum;
import org.siemac.metamac.statistical.operations.core.stream.mappers.Do2AvroMapper;
import org.siemac.metamac.statistical.operations.core.stream.messages.StatusEnumAvro;
import org.springframework.stereotype.Component;

@Component
public class StatusDo2AvroMapper implements Do2AvroMapper<StatusEnum, StatusEnumAvro> {
    @Override
    public StatusEnumAvro toAvro(StatusEnum source) {
        if (source == null) {
            return null;
        }
        return StatusEnumAvro.valueOf(source.name());
    }
}
