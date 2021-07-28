package org.siemac.metamac.statistical.operations.core.stream.mappers.impl;

import org.siemac.metamac.core.common.ent.domain.ExternalItem;
import org.siemac.metamac.statistical.operations.core.stream.mappers.Do2AvroMapper;
import org.siemac.metamac.statistical.operations.core.stream.messages.ExternalItemAvro;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class ExternalItemDo2AvroMapper implements Do2AvroMapper<ExternalItem, ExternalItemAvro> {
    @Autowired
    InternationalStringDo2AvroMapper internationalStringDo2AvroMapper;

    @Autowired
    TypeExternalArtifactsDo2AvroMapper typeExternalArtifactsDo2AvroMapper;

    @Override
    public ExternalItemAvro toAvro(ExternalItem source) {
        if (source == null) {
            return null;
        }
        return ExternalItemAvro.newBuilder()
                               .setCode(source.getCode())
                               .setCodeNested(source.getCodeNested())
                               .setUrn(source.getUrn())
                               .setUrnProvider(source.getUrnProvider())
                               .setManagementAppUrl(source.getManagementAppUrl())
                               .setType(typeExternalArtifactsDo2AvroMapper.toAvro(source.getType()))
                               .setTitle(internationalStringDo2AvroMapper.toAvro(source.getTitle()))
                               .build();
    }
}
