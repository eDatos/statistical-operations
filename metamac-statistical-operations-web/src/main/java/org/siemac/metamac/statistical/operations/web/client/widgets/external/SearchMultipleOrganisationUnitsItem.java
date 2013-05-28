package org.siemac.metamac.statistical.operations.web.client.widgets.external;

import static org.siemac.metamac.statistical.operations.web.client.OperationsWeb.getConstants;

import org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum;

public class SearchMultipleOrganisationUnitsItem extends SearchMultipleItemsItem {

    public SearchMultipleOrganisationUnitsItem(String name, String title, MultipleExternalResourceAction action) {
        super(name, title, TypeExternalArtefactsEnum.ORGANISATION_UNIT_SCHEME, TypeExternalArtefactsEnum.ORGANISATION_UNIT, getConstants().searchOrganisations(), getConstants()
                .filterOrganisationScheme(), getConstants().selectedOrganisationScheme(), getConstants().selectionOrganisations(), action);
    }
}
