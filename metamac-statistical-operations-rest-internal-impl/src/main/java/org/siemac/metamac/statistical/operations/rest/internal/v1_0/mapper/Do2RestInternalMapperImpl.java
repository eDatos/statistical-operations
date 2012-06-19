package org.siemac.metamac.statistical.operations.rest.internal.v1_0.mapper;

import java.util.ArrayList; 
import java.util.List;

import org.siemac.metamac.core.common.ent.domain.InternationalString;
import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.LocalisedString;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Operation;
import org.springframework.stereotype.Component;

@Component
public class Do2RestInternalMapperImpl implements Do2RestInternalMapper {

    @Override
    public Operation operationToOperationBase(org.siemac.metamac.statistical.operations.core.domain.Operation source) throws MetamacException {
        if (source == null) {
            return null;
        }
        Operation operation = new Operation();
        operation.setCode(source.getCode());
        if (source.getTitle() != null) {
            operation.getTitles().addAll(internationalStringToText(source.getTitle()));
        }

        return operation;
    }

    private List<LocalisedString> internationalStringToText(InternationalString sources) {
        if (sources == null) {
            return null;
        }
        List<LocalisedString> targets = new ArrayList<LocalisedString>();
        for (org.siemac.metamac.core.common.ent.domain.LocalisedString source : sources.getTexts()) {
            LocalisedString target = new LocalisedString();
            target.setLabel(source.getLabel());
            target.setLocale(source.getLocale());
            targets.add(target);
        }
        return targets;
    }
}
