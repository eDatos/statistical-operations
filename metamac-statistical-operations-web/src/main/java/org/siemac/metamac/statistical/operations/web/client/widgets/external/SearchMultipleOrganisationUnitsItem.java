package org.siemac.metamac.statistical.operations.web.client.widgets.external;

import static org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum.ORGANISATION_UNIT;
import static org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum.ORGANISATION_UNIT_SCHEME;
import static org.siemac.metamac.statistical.operations.web.client.OperationsWeb.getConstants;

import org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum;
import org.siemac.metamac.statistical.operations.web.shared.external.RestWebCriteriaUtils;

public class SearchMultipleOrganisationUnitsItem extends SearchMultipleItemsItem {

    public SearchMultipleOrganisationUnitsItem(String name, String title, MultipleExternalResourceAction action) {
        super(name, title, RestWebCriteriaUtils.buildItemSchemeWebCriteria(ORGANISATION_UNIT_SCHEME), RestWebCriteriaUtils.buildItemWebCriteria(new TypeExternalArtefactsEnum[]{ORGANISATION_UNIT},
                null, null), getConstants().searchOrganisations(), getConstants().filterOrganisationScheme(), getConstants().selectedOrganisationScheme(), getConstants().selectionOrganisations(),
                action);
    }
}