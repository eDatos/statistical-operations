package org.siemac.metamac.statistical.operations.web.client.widgets.external;

import static org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum.DATA_PROVIDER;
import static org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum.DATA_PROVIDER_SCHEME;
import static org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum.ORGANISATION_UNIT;
import static org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum.ORGANISATION_UNIT_SCHEME;
import static org.siemac.metamac.statistical.operations.web.client.OperationsWeb.getConstants;

import org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum;
import org.siemac.metamac.statistical.operations.web.shared.external.RestWebCriteriaUtils;

public class SearchMultipleOrganisationUnitsAndDataProvidersItem extends SearchMultipleItemsItem {

    public SearchMultipleOrganisationUnitsAndDataProvidersItem(String name, String title, MultipleExternalResourceAction action) {
        super(name, title, RestWebCriteriaUtils.buildItemSchemeWebCriteria(ORGANISATION_UNIT_SCHEME, DATA_PROVIDER_SCHEME), RestWebCriteriaUtils.buildItemWebCriteria(new TypeExternalArtefactsEnum[]{
                ORGANISATION_UNIT, DATA_PROVIDER}, null, null), getConstants().searchOrganisations(), getConstants().filterOrganisationScheme(), getConstants().selectedOrganisationScheme(),
                getConstants().selectionOrganisations(), action);
    }
}