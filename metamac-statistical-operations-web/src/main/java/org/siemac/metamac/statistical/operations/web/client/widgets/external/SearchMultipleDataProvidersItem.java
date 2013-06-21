package org.siemac.metamac.statistical.operations.web.client.widgets.external;

import static org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum.DATA_PROVIDER;
import static org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum.DATA_PROVIDER_SCHEME;
import static org.siemac.metamac.statistical.operations.web.client.OperationsWeb.getConstants;

import org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum;
import org.siemac.metamac.statistical.operations.web.shared.external.RestWebCriteriaUtils;
import org.siemac.metamac.web.common.client.widgets.form.fields.external.MultipleExternalResourceAction;
import org.siemac.metamac.web.common.client.widgets.form.fields.external.SearchMultipleSrmItemsItem;

public class SearchMultipleDataProvidersItem extends SearchMultipleSrmItemsItem {

    public SearchMultipleDataProvidersItem(String name, String title, MultipleExternalResourceAction action) {
        super(name, title, RestWebCriteriaUtils.buildItemSchemeWebCriteria(DATA_PROVIDER_SCHEME),
                RestWebCriteriaUtils.buildItemWebCriteria(new TypeExternalArtefactsEnum[]{DATA_PROVIDER}, null, null), getConstants().searchOrganisations(), getConstants().filterOrganisationScheme(),
                getConstants().selectedOrganisationScheme(), getConstants().selectionOrganisations(), action);
    }
}
