package org.siemac.metamac.statistical.operations.web.client.widgets.external;

import static org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum.DATA_PROVIDER;
import static org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum.DATA_PROVIDER_SCHEME;
import static org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum.ORGANISATION_UNIT;
import static org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum.ORGANISATION_UNIT_SCHEME;
import static org.siemac.metamac.statistical.operations.web.client.OperationsWeb.getConstants;

import org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum;

public class SearchMultipleOrganisationUnitsAndDataProvidersItem extends SearchMultipleItemsItem {

    public SearchMultipleOrganisationUnitsAndDataProvidersItem(String name, String title, MultipleExternalResourceAction action) {
        super(name, title, new TypeExternalArtefactsEnum[]{ORGANISATION_UNIT_SCHEME, DATA_PROVIDER_SCHEME}, new TypeExternalArtefactsEnum[]{ORGANISATION_UNIT, DATA_PROVIDER}, getConstants()
                .searchOrganisations(), getConstants().filterOrganisationScheme(), getConstants().selectedOrganisationScheme(), getConstants().selectionOrganisations(), action);
    }
}
