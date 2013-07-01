package org.siemac.metamac.statistical.operations.web.client.widgets.external;

import static org.siemac.metamac.statistical.operations.web.client.OperationsWeb.getConstants;

import java.util.LinkedHashMap;

import org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum;
import org.siemac.metamac.web.common.client.widgets.form.fields.CustomSelectItem;

public class ConceptAndConceptSchemeSelectItem extends CustomSelectItem {

    public ConceptAndConceptSchemeSelectItem(TypeExternalArtefactsEnum initialValue) {
        setTitle(getConstants().resourceTypeToAdd());
        setTitleStyle("staticFormItemTitle");
        setValueMap(getConceptAndConceptSchemeValueMap());
        setValue(initialValue.name());
    }

    private LinkedHashMap<String, String> getConceptAndConceptSchemeValueMap() {
        LinkedHashMap<String, String> valueMap = new LinkedHashMap<String, String>();
        valueMap.put(TypeExternalArtefactsEnum.CONCEPT_SCHEME.name(), getConstants().conceptSchemes());
        valueMap.put(TypeExternalArtefactsEnum.CONCEPT.name(), getConstants().concepts());
        return valueMap;
    }
}
