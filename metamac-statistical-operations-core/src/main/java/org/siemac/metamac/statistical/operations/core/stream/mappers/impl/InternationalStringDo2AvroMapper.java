package org.siemac.metamac.statistical.operations.core.stream.mappers.impl;

import java.util.ArrayList;
import java.util.List;

import org.siemac.metamac.core.common.ent.domain.InternationalString;
import org.siemac.metamac.core.common.ent.domain.LocalisedString;
import org.siemac.metamac.statistical.operations.core.stream.mappers.Do2AvroMapper;
import org.siemac.metamac.statistical.operations.core.stream.messages.InternationalStringAvro;
import org.siemac.metamac.statistical.operations.core.stream.messages.LocalisedStringAvro;
import org.springframework.stereotype.Component;

@Component
public class InternationalStringDo2AvroMapper implements Do2AvroMapper<InternationalString, InternationalStringAvro> {
    @Override
    public InternationalStringAvro toAvro(InternationalString source) {
        InternationalStringAvro target = null;
        if (source != null && source.getTexts() != null) {
            List<LocalisedStringAvro> avroLocalisedStrings = new ArrayList<>();
            for (LocalisedString localisedString : source.getTexts()) {
                LocalisedStringAvro avroLocalisedString = LocalisedStringAvro.newBuilder()
                                                                             .setLocale(localisedString.getLocale())
                                                                             .setLabel(localisedString.getLabel())
                                                                             .build();
                avroLocalisedStrings.add(avroLocalisedString);
            }
            target = InternationalStringAvro.newBuilder().setLocalisedStrings(avroLocalisedStrings).build();
        }
        return target;
    }
}
