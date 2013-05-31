package org.siemac.metamac.statistical.operations.web.client.widgets.external;

import static org.siemac.metamac.statistical.operations.web.client.OperationsWeb.getConstants;

import org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum;
import org.siemac.metamac.statistical.operations.web.shared.external.ExternalResourceWebCriteria;
import org.siemac.metamac.statistical.operations.web.shared.external.ItemWebCriteria;
import org.siemac.metamac.web.common.shared.domain.ExternalItemsResult;

import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;

public class SearchMultipleConceptsAndConceptSchemesItem extends SearchMultipleItemsItem {

    private TypeExternalArtefactsEnum   selectedResourceType;

    private ExternalResourceWebCriteria itemSchemeCriteria;
    private ItemWebCriteria             itemCriteria;

    public SearchMultipleConceptsAndConceptSchemesItem(final String name, String title, ExternalResourceWebCriteria itemSchemeCriteria, ItemWebCriteria itemCriteria,
            MultipleExternalResourceAction action) {
        super(name, title, itemSchemeCriteria, itemCriteria, getConstants().searchConceptsAndConceptSchemes(), getConstants().filterConceptScheme(), getConstants().selectedConceptScheme(),
                getConstants().selectionResources(), new ConceptAndConceptSchemeSelectItem(), action);

        this.itemSchemeCriteria = itemSchemeCriteria;
        this.itemCriteria = itemCriteria;

        getInitialFilterSelectItem().addChangedHandler(new ChangedHandler() {

            @Override
            public void onChanged(ChangedEvent event) {
                if (event.getValue() != null) {

                    SearchMultipleConceptsAndConceptSchemesItem.this.itemSchemeCriteria.setCriteria(null);
                    SearchMultipleConceptsAndConceptSchemesItem.this.itemCriteria.setCriteria(null);
                    SearchMultipleConceptsAndConceptSchemesItem.this.itemCriteria.setItemSchemUrn(null);
                    searchMultipleItemsWindow.clearListSearchSections();

                    selectedResourceType = TypeExternalArtefactsEnum.valueOf((String) event.getValue());

                    if (TypeExternalArtefactsEnum.CONCEPT_SCHEME.equals(selectedResourceType)) {
                        searchMultipleItemsWindow.getFilterForm().hide();
                        getUiHandlers().retrieveItemSchemes(name, SearchMultipleConceptsAndConceptSchemesItem.this.itemSchemeCriteria, FIRST_RESULT, MAX_RESULTS);
                    }
                    if (TypeExternalArtefactsEnum.CONCEPT.equals(selectedResourceType)) {
                        searchMultipleItemsWindow.getFilterForm().show();
                        getUiHandlers().retrieveItemSchemes(name, SearchMultipleConceptsAndConceptSchemesItem.this.itemSchemeCriteria, FIRST_RESULT, MAX_RESULTS);
                        getUiHandlers().retrieveItems(name, SearchMultipleConceptsAndConceptSchemesItem.this.itemCriteria, FIRST_RESULT, MAX_RESULTS);
                    }

                    searchMultipleItemsWindow.markForRedraw();
                }
            }
        });
    }

    /**
     * This method is overwritten because we are showing item schemes in the list where items are usually shown
     */
    @Override
    protected void retrieveItems(String formItemName, ItemWebCriteria itemWebCriteria, int firstResult, int maxResults) {
        if (TypeExternalArtefactsEnum.CONCEPT_SCHEME.equals(selectedResourceType)) {
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
        if (TypeExternalArtefactsEnum.CONCEPT_SCHEME.equals(selectedResourceType)) {
            super.setItems(result);
        } else {
            super.setItemSchemes(result);
        }
    }
}
