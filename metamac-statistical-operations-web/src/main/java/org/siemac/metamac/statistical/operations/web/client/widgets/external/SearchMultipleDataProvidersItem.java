package org.siemac.metamac.statistical.operations.web.client.widgets.external;

import static org.siemac.metamac.statistical.operations.web.client.OperationsWeb.getConstants;

import org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum;

public class SearchMultipleDataProvidersItem extends SearchMultipleItemsItem {

    public SearchMultipleDataProvidersItem(String name, String title, MultipleExternalResourceAction action) {
        super(name, title, TypeExternalArtefactsEnum.DATA_PROVIDER_SCHEME, TypeExternalArtefactsEnum.DATA_PROVIDER, getConstants().searchOrganisations(), getConstants().filterOrganisationScheme(),
                getConstants().selectedOrganisationScheme(), getConstants().selectionOrganisations(), action);
    }
}
