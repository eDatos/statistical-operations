package org.siemac.metamac.statistical.operations.web.client.widgets.external;

import static org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum.CATEGORY;
import static org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum.CATEGORY_SCHEME;
import static org.siemac.metamac.statistical.operations.web.client.OperationsWeb.getConstants;

import org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum;

public class SearchMultipleCategoriesItem extends SearchMultipleItemsItem {

    public SearchMultipleCategoriesItem(String name, String title, MultipleExternalResourceAction action) {
        super(name, title, new TypeExternalArtefactsEnum[]{CATEGORY_SCHEME}, new TypeExternalArtefactsEnum[]{CATEGORY}, getConstants().searchCategories(), getConstants().filterCategoryScheme(),
                getConstants().selectedCategoryScheme(), getConstants().selectionCategories(), action);
    }
}
