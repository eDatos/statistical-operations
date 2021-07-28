package org.siemac.metamac.statistical.operations.core.stream.mappers.impl;

import org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum;
import org.siemac.metamac.statistical.operations.core.stream.mappers.Do2AvroMapper;
import org.siemac.metamac.statistical.operations.core.stream.messages.TypeExternalArtefactsEnumAvro;
import org.springframework.stereotype.Component;

@Component
public class TypeExternalArtifactsDo2AvroMapper implements Do2AvroMapper<TypeExternalArtefactsEnum, TypeExternalArtefactsEnumAvro> {
    @Override
    public TypeExternalArtefactsEnumAvro toAvro(TypeExternalArtefactsEnum source) {
        if (source == null) {
            return null;
        }
        return TypeExternalArtefactsEnumAvro.valueOf(source.name());
    }
}
