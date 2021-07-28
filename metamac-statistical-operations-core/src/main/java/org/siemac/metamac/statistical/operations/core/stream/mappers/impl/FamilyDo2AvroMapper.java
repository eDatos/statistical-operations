package org.siemac.metamac.statistical.operations.core.stream.mappers.impl;

import org.siemac.metamac.statistical.operations.core.domain.Family;
import org.siemac.metamac.statistical.operations.core.stream.mappers.Do2AvroMapper;
import org.siemac.metamac.statistical.operations.core.stream.messages.FamilyAvro;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class FamilyDo2AvroMapper implements Do2AvroMapper<Family, FamilyAvro> {
    @Autowired
    DatetimeDo2AvroMapper datetimeDo2AvroMapper;

    @Autowired
    InternationalStringDo2AvroMapper internationalStringDo2AvroMapper;

    @Autowired
    ProcStatusDo2AvroMapper procStatusDo2AvroMapper;

    @Override
    public FamilyAvro toAvro(Family source) {
        if (source == null) {
            return null;
        }
        return FamilyAvro.newBuilder()
                         .setCode(source.getCode())
                         .setUrn(source.getUrn())
                         .setInternalInventoryDate(datetimeDo2AvroMapper.toAvro(source.getInternalInventoryDate()))
                         .setInventoryDate(datetimeDo2AvroMapper.toAvro(source.getInventoryDate()))
                         .setUpdateDate(datetimeDo2AvroMapper.toAvro(source.getUpdateDate()))
                         .setUuid(source.getUuid())
                         .setCreatedDate(datetimeDo2AvroMapper.toAvro(source.getCreatedDate()))
                         .setCreatedBy(source.getCreatedBy())
                         .setLastUpdated(datetimeDo2AvroMapper.toAvro(source.getLastUpdated()))
                         .setLastUpdatedBy(source.getLastUpdatedBy())
                         .setVersion(source.getVersion())
                         .setTitle(internationalStringDo2AvroMapper.toAvro(source.getTitle()))
                         .setAcronym(internationalStringDo2AvroMapper.toAvro(source.getAcronym()))
                         .setDescription(internationalStringDo2AvroMapper.toAvro(source.getDescription()))
                         .setProcStatus(procStatusDo2AvroMapper.toAvro(source.getProcStatus()))
                         .build();
    }
}
