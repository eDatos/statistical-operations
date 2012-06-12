package org.siemac.metamac.statistical.operations.core.mapper.ws;

import java.math.BigInteger;

import org.siemac.metamac.schema.common.v1_0.domain.InternationalString;
import org.siemac.metamac.schema.common.v1_0.domain.LocalisedString;
import org.siemac.metamac.schema.common.v1_0.domain.LocalisedStringList;
import org.springframework.stereotype.Component;

@Component
public class Do2WebServiceCommonMapperImpl implements Do2WebServiceCommonMapper {

    public InternationalString internationalStringToWebService(org.siemac.metamac.core.common.ent.domain.InternationalString source) {
        if (source == null || source.getTexts() == null || source.getTexts().size() == 0) {
            return null;
        }
        
        InternationalString internationalString = new InternationalString();
        internationalString.setLocalisedStrings(new LocalisedStringList());
        internationalString.getLocalisedStrings().setTotal(BigInteger.valueOf(source.getTexts().size()));
        // LocalisedString to LocalisedString Ws
        for (org.siemac.metamac.core.common.ent.domain.LocalisedString item : source.getTexts()) {
            LocalisedString localisedString = localisedStringToLocalisedStringWebService(item);
            internationalString.getLocalisedStrings().getLocalisedString().add(localisedString);
        }

        return internationalString;
    }

    private LocalisedString localisedStringToLocalisedStringWebService(org.siemac.metamac.core.common.ent.domain.LocalisedString source) {
        LocalisedString localisedString = new LocalisedString();
        localisedString.setLocale(source.getLocale());
        localisedString.setLabel(source.getLabel());
        return localisedString;
    }
}
