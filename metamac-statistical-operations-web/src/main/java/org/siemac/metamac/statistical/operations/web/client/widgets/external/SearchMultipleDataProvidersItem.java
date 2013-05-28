package org.siemac.metamac.statistical.operations.web.client.widgets.external;

import static org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum.DATA_PROVIDER;
import static org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum.DATA_PROVIDER_SCHEME;
import static org.siemac.metamac.statistical.operations.web.client.OperationsWeb.getConstants;

import org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum;

public class SearchMultipleDataProvidersItem extends SearchMultipleItemsItem {

    public SearchMultipleDataProvidersItem(String name, String title, MultipleExternalResourceAction action) {
        super(name, title, new TypeExternalArtefactsEnum[]{DATA_PROVIDER_SCHEME}, new TypeExternalArtefactsEnum[]{DATA_PROVIDER}, getConstants().searchOrganisations(), getConstants()
                .filterOrganisationScheme(), getConstants().selectedOrganisationScheme(), getConstants().selectionOrganisations(), action);
    }
}
