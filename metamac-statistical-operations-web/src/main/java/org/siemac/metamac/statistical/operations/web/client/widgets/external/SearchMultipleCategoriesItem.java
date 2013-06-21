package org.siemac.metamac.statistical.operations.web.client.widgets.external;

import static org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum.CATEGORY;
import static org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum.CATEGORY_SCHEME;
import static org.siemac.metamac.statistical.operations.web.client.OperationsWeb.getConstants;

import org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum;
import org.siemac.metamac.statistical.operations.web.shared.external.RestWebCriteriaUtils;
import org.siemac.metamac.web.common.client.widgets.form.fields.external.MultipleExternalResourceAction;
import org.siemac.metamac.web.common.client.widgets.form.fields.external.SearchMultipleSrmItemsItem;
import org.siemac.metamac.web.common.shared.criteria.SrmItemRestCriteria;

public class SearchMultipleCategoriesItem extends SearchMultipleSrmItemsItem {

    public SearchMultipleCategoriesItem(String name, String title, MultipleExternalResourceAction action) {
        super(name, title, RestWebCriteriaUtils.buildItemSchemeWebCriteria(CATEGORY_SCHEME), RestWebCriteriaUtils.buildItemWebCriteria(new SrmItemRestCriteria(),
                new TypeExternalArtefactsEnum[]{CATEGORY}), getConstants().searchCategories(), getConstants().filterCategoryScheme(), getConstants().selectedCategoryScheme(), getConstants()
                .selectionCategories(), action);
    }
}
