package org.siemac.metamac.statistical.operations.web.client.widgets.external;

import static org.siemac.metamac.statistical.operations.web.client.OperationsWeb.getConstants;

import org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum;
import org.siemac.metamac.core.common.util.shared.StringUtils;
import org.siemac.metamac.web.common.client.widgets.form.fields.external.MultipleExternalResourceAction;
import org.siemac.metamac.web.common.client.widgets.form.fields.external.SearchMultipleSrmItemsItem;
import org.siemac.metamac.web.common.shared.criteria.SrmItemRestCriteria;
import org.siemac.metamac.web.common.shared.criteria.SrmItemSchemeRestCriteria;
import org.siemac.metamac.web.common.shared.domain.ExternalItemsResult;

import com.smartgwt.client.widgets.form.fields.events.ChangeEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangeHandler;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.form.fields.events.ClickEvent;
import com.smartgwt.client.widgets.form.fields.events.ClickHandler;

public class SearchMultipleConceptsAndConceptSchemesItem extends SearchMultipleSrmItemsItem {

    private static final TypeExternalArtefactsEnum DEFAULT_INITIAL_RESOURCE_TYPE = TypeExternalArtefactsEnum.CONCEPT;

    private SrmItemSchemeRestCriteria              itemSchemeCriteria;
    private SrmItemRestCriteria                    itemCriteria;

    public SearchMultipleConceptsAndConceptSchemesItem(final String name, String title, SrmItemSchemeRestCriteria itemSchemeCriteria, SrmItemRestCriteria itemCriteria,
            MultipleExternalResourceAction action) {
        super(name, title, itemSchemeCriteria, itemCriteria, getConstants().searchConceptsAndConceptSchemes(), getConstants().filterConceptScheme(), getConstants().selectedConceptScheme(),
                getConstants().selectionResources(), new ConceptAndConceptSchemeSelectItem(DEFAULT_INITIAL_RESOURCE_TYPE), action);

        this.itemSchemeCriteria = itemSchemeCriteria;
        this.itemCriteria = itemCriteria;

        getInitialFilterSelectItem().addChangeHandler(new ChangeHandler() {

            @Override
            public void onChange(ChangeEvent event) {
                System.out.println();
            }
        });

        getInitialFilterSelectItem().addClickHandler(new ClickHandler() {

            @Override
            public void onClick(ClickEvent event) {
                System.out.println();
            }
        });

        getInitialFilterSelectItem().addChangedHandler(new ChangedHandler() {

            @Override
            public void onChanged(ChangedEvent event) {
                if (event.getValue() != null) {

                    SearchMultipleConceptsAndConceptSchemesItem.this.itemSchemeCriteria.setCriteria(null);
                    SearchMultipleConceptsAndConceptSchemesItem.this.itemCriteria.setCriteria(null);
                    SearchMultipleConceptsAndConceptSchemesItem.this.itemCriteria.setItemSchemeUrn(null);
                    searchMultipleItemsWindow.clearListSearchSections();

                    TypeExternalArtefactsEnum selectedResourceType = getSelectedTypeExternalArtefactsEnum();

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
        String value = getInitialFilterSelectItem().getValueAsString();
        if (!StringUtils.isBlank(value)) {
            try {
                return TypeExternalArtefactsEnum.valueOf(value);
            } catch (Exception e) {
                return null;
            }
        }
        return null;
    }
}
