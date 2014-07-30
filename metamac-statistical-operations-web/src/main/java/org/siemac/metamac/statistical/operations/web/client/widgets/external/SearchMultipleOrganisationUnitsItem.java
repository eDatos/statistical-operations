package org.siemac.metamac.statistical.operations.web.client.widgets.external;

import static org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum.ORGANISATION_UNIT;
import static org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum.ORGANISATION_UNIT_SCHEME;
import static org.siemac.metamac.statistical.operations.web.client.OperationsWeb.getConstants;

import org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum;
import org.siemac.metamac.statistical.operations.web.shared.external.RestWebCriteriaUtils;
import org.siemac.metamac.web.common.client.widgets.form.fields.external.MultipleExternalResourceAction;
import org.siemac.metamac.web.common.client.widgets.form.fields.external.SearchMultipleSrmItemsItem;
import org.siemac.metamac.web.common.shared.criteria.SrmItemRestCriteria;

public class SearchMultipleOrganisationUnitsItem extends SearchMultipleSrmItemsItem {

    @Deprecated
    public SearchMultipleOrganisationUnitsItem(String name, String title, MultipleExternalResourceAction action) {
        super(name, title, RestWebCriteriaUtils.buildItemSchemeWebCriteria(ORGANISATION_UNIT_SCHEME), RestWebCriteriaUtils.buildItemWebCriteria(new SrmItemRestCriteria(),
                new TypeExternalArtefactsEnum[]{ORGANISATION_UNIT}), getConstants().searchOrganisations(), getConstants().filterOrganisationScheme(), getConstants().selectedOrganisationScheme(),
                getConstants().selectionOrganisations(), action);
    }
}
