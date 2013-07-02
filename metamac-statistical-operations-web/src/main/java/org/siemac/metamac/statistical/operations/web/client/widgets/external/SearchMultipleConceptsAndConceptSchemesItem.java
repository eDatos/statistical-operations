package org.siemac.metamac.statistical.operations.web.client.widgets.external;

import static org.siemac.metamac.statistical.operations.web.client.OperationsWeb.getConstants;

import java.util.LinkedHashMap;

import org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum;
import org.siemac.metamac.core.common.util.shared.StringUtils;
import org.siemac.metamac.web.common.client.widgets.form.fields.CustomSelectItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.external.MultipleExternalResourceAction;
import org.siemac.metamac.web.common.client.widgets.form.fields.external.SearchMultipleSrmItemsItem;
import org.siemac.metamac.web.common.shared.criteria.SrmItemRestCriteria;
import org.siemac.metamac.web.common.shared.criteria.SrmItemSchemeRestCriteria;
import org.siemac.metamac.web.common.shared.domain.ExternalItemsResult;

import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;

public class SearchMultipleConceptsAndConceptSchemesItem extends SearchMultipleSrmItemsItem {

    private static final String       RESOURCE_TYPE_FIELD_NAME = "resource-type";

    private SrmItemSchemeRestCriteria itemSchemeCriteria;
    private SrmItemRestCriteria       itemCriteria;

    public SearchMultipleConceptsAndConceptSchemesItem(final String name, String title, SrmItemSchemeRestCriteria itemSchemeCriteria, SrmItemRestCriteria itemCriteria,
            MultipleExternalResourceAction action) {
        super(name, title, itemSchemeCriteria, itemCriteria, getConstants().searchConceptsAndConceptSchemes(), getConstants().filterConceptScheme(), getConstants().selectedConceptScheme(),
                getConstants().selectionResources(), action);

        this.itemSchemeCriteria = itemSchemeCriteria;
        this.itemCriteria = itemCriteria;
    }

    @Override
    public FormItem[] getFormItemtsToAddToInitialFilterForm() {
        CustomSelectItem resourceType = new CustomSelectItem(RESOURCE_TYPE_FIELD_NAME, getConstants().resourceTypeToAdd());
        resourceType.setValueMap(getConceptAndConceptSchemeValueMap());
        resourceType.setValue(TypeExternalArtefactsEnum.CONCEPT.name());
        resourceType.setVisible(true);
        resourceType.addChangedHandler(new ChangedHandler() {

            @Override
            public void onChanged(ChangedEvent event) {
                if (event.getValue() != null) {

                    SearchMultipleConceptsAndConceptSchemesItem.this.itemSchemeCriteria.setCriteria(null);
                    SearchMultipleConceptsAndConceptSchemesItem.this.itemCriteria.setCriteria(null);
                    SearchMultipleConceptsAndConceptSchemesItem.this.itemCriteria.setItemSchemeUrn(null);
                    searchMultipleItemsWindow.clearListSearchSections();
                    SearchMultipleConceptsAndConceptSchemesItem.this.clearDefaultItemScheme(SearchMultipleConceptsAndConceptSchemesItem.this.itemSchemeCriteria,
                            SearchMultipleConceptsAndConceptSchemesItem.this.itemCriteria);

                    TypeExternalArtefactsEnum selectedResourceType = getSelectedTypeExternalArtefactsEnum();

                    if (TypeExternalArtefactsEnum.CONCEPT_SCHEME.equals(selectedResourceType)) {
                        searchMultipleItemsWindow.getFilterForm().hide();
                        getUiHandlers().retrieveItemSchemes(getName(), SearchMultipleConceptsAndConceptSchemesItem.this.itemSchemeCriteria, FIRST_RESULT, MAX_RESULTS);
                    }
                    if (TypeExternalArtefactsEnum.CONCEPT.equals(selectedResourceType)) {
                        searchMultipleItemsWindow.getFilterForm().show();
                        getUiHandlers().retrieveItemSchemes(getName(), SearchMultipleConceptsAndConceptSchemesItem.this.itemSchemeCriteria, FIRST_RESULT, MAX_RESULTS);
                        getUiHandlers().retrieveItems(getName(), SearchMultipleConceptsAndConceptSchemesItem.this.itemCriteria, FIRST_RESULT, MAX_RESULTS);
                    }

                    searchMultipleItemsWindow.markForRedraw();
                }
            }
        });
        FormItem[] formItems = new FormItem[]{resourceType};
        return formItems;
    }

    /**
     * This method is overwritten because we are showing item schemes in the list where items are usually shown
     */
    @Override
    protected void retrieveItems(String formItemName, SrmItemRestCriteria itemWebCriteria, int firstResult, int maxResults) {
        if (TypeExternalArtefactsEnum.CONCEPT_SCHEME.equals(getSelectedTypeExternalArtefactsEnum())) {
            itemSchemeCriteria.setCriteria(searchMultipleItemsWindow.getSelectionListCriteria());
            getUiHandlers().retrieveItemSchemes(formItemName, itemSchemeCriteria, firstResult, maxResults);
        } else {
            super.retrieveItems(formItemName, itemWebCriteria, firstResult, maxResults);
        }
    }

    /**
     * This method is overwritten because we want to show item schemes in the list where items are usually shown
     */
    @Override
    public void setItemSchemes(ExternalItemsResult result) {
        if (TypeExternalArtefactsEnum.CONCEPT_SCHEME.equals(getSelectedTypeExternalArtefactsEnum())) {
            super.setItems(result);
        } else {
            super.setItemSchemes(result);
        }
    }

    private TypeExternalArtefactsEnum getSelectedTypeExternalArtefactsEnum() {
        if (searchMultipleItemsWindow.getInitialFilterForm().getItem(RESOURCE_TYPE_FIELD_NAME).getValue() != null) {
            String value = (String) searchMultipleItemsWindow.getInitialFilterForm().getItem(RESOURCE_TYPE_FIELD_NAME).getValue();
            if (!StringUtils.isBlank(value)) {
                try {
                    return TypeExternalArtefactsEnum.valueOf(value);
                } catch (Exception e) {
                    return null;
                }
            }
        }
        return null;
    }

    private LinkedHashMap<String, String> getConceptAndConceptSchemeValueMap() {
        LinkedHashMap<String, String> valueMap = new LinkedHashMap<String, String>();
        valueMap.put(TypeExternalArtefactsEnum.CONCEPT_SCHEME.name(), getConstants().conceptSchemes());
        valueMap.put(TypeExternalArtefactsEnum.CONCEPT.name(), getConstants().concepts());
        return valueMap;
    }
}
