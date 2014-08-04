package org.siemac.metamac.statistical.operations.web.client.widgets.external;

import static org.siemac.metamac.statistical.operations.web.client.OperationsWeb.getConstants;

import java.util.LinkedHashMap;
import java.util.List;

import org.siemac.metamac.core.common.dto.ExternalItemDto;
import org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum;
import org.siemac.metamac.core.common.util.shared.StringUtils;
import org.siemac.metamac.statistical.operations.web.client.constants.StatisticalOperationsWebConstants;
import org.siemac.metamac.statistical.operations.web.client.instance.view.handlers.InstanceUiHandlers;
import org.siemac.metamac.statistical.operations.web.shared.external.ConceptSchemeTypeEnum;
import org.siemac.metamac.web.common.client.MetamacWebCommon;
import org.siemac.metamac.web.common.client.widgets.actions.search.SearchPaginatedAction;
import org.siemac.metamac.web.common.client.widgets.form.CustomDynamicForm;
import org.siemac.metamac.web.common.client.widgets.form.fields.CustomSelectItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.external.ExternalItemListItem;
import org.siemac.metamac.web.common.client.widgets.windows.search.SearchMultipleSrmItemWithSchemeFilterPaginatedWindow;
import org.siemac.metamac.web.common.shared.criteria.SrmExternalResourceRestCriteria;
import org.siemac.metamac.web.common.shared.criteria.SrmItemRestCriteria;
import org.siemac.metamac.web.common.shared.domain.ExternalItemsResult;

import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.form.fields.events.ClickEvent;
import com.smartgwt.client.widgets.form.fields.events.ClickHandler;
import com.smartgwt.client.widgets.form.fields.events.FormItemClickHandler;
import com.smartgwt.client.widgets.form.fields.events.FormItemIconClickEvent;

public abstract class SearchSrmListConceptAndConceptSchemeItem extends ExternalItemListItem {

    private SearchMultipleSrmItemWithSchemeFilterPaginatedWindow window;
    private CustomDynamicForm                                    resourceTypeSelectorForm;
    protected InstanceUiHandlers                                 uiHandlers;

    private static final String                                  RESOURCE_TYPE_FIELD_NAME = "resource-type";

    public SearchSrmListConceptAndConceptSchemeItem(String name, String title, int maxResults) {
        super(name, title, true);
        appendWindow(maxResults);
    }

    private void appendWindow(final int maxResults) {

        getSearchIcon().addFormItemClickHandler(new FormItemClickHandler() {

            @Override
            public void onFormItemClick(FormItemIconClickEvent event) {

                SearchPaginatedAction<SrmExternalResourceRestCriteria> filterSearchAction = new SearchPaginatedAction<SrmExternalResourceRestCriteria>() {

                    @Override
                    public void retrieveResultSet(int firstResult, int maxResults, SrmExternalResourceRestCriteria webCriteria) {
                        webCriteria.setOnlyLastVersion(window.getFilter().getSearchCriteria().isItemSchemeLastVersion());
                        retrieveItemSchemes(firstResult, maxResults, webCriteria);
                    }
                };

                createResourceTypeSelectorForm();

                window = new SearchMultipleSrmItemWithSchemeFilterPaginatedWindow(MetamacWebCommon.getConstants().resourceSelection(), maxResults, filterSearchAction,
                        new SearchPaginatedAction<SrmItemRestCriteria>() {

                            @Override
                            public void retrieveResultSet(int firstResult, int maxResults, SrmItemRestCriteria webCriteria) {
                                retrieveItems(firstResult, maxResults, webCriteria);
                            }
                        });

                window.getSearchWindowLayout().addMember(createResourceTypeSelectorForm(), 0);

                window.retrieveItems();

                window.setSelectedResources(getSelectedRelatedResources());

                window.setSaveAction(new ClickHandler() {

                    @Override
                    public void onClick(ClickEvent event) {
                        setExternalItems(window.getSelectedResources());
                        window.markForDestroy();
                    }
                });
            }
        });
    }

    private CustomDynamicForm createResourceTypeSelectorForm() {
        resourceTypeSelectorForm = new CustomDynamicForm();
        resourceTypeSelectorForm.setWidth(500);
        resourceTypeSelectorForm.setMargin(15);
        CustomSelectItem resourceTypeItem = new CustomSelectItem(RESOURCE_TYPE_FIELD_NAME, getConstants().resourceTypeToAdd());
        resourceTypeItem.setValueMap(getConceptAndConceptSchemeValueMap());
        resourceTypeItem.setValue(TypeExternalArtefactsEnum.CONCEPT.name());
        resourceTypeItem.addChangedHandler(new ChangedHandler() {

            @Override
            public void onChanged(ChangedEvent event) {

                TypeExternalArtefactsEnum selectedResourceType = getSelectedTypeExternalArtefactsEnum();

                SrmItemRestCriteria criteria = new SrmItemRestCriteria();
                criteria.setCriteria(window.getFilter().getSearchCriteria().getCriteria());
                criteria.setItemSchemeLastVersion(window.getFilter().getSearchCriteria().isItemSchemeLastVersion());

                if (TypeExternalArtefactsEnum.CONCEPT_SCHEME.equals(selectedResourceType)) {
                    window.getFilter().getItemSchemeFilterFacet().getFormItem().setVisible(false);
                    criteria.setExternalArtifactType(TypeExternalArtefactsEnum.CONCEPT_SCHEME);
                }
                if (TypeExternalArtefactsEnum.CONCEPT.equals(selectedResourceType)) {
                    window.getFilter().getItemSchemeFilterFacet().getFormItem().setVisible(true);
                    criteria.setExternalArtifactType(TypeExternalArtefactsEnum.CONCEPT);
                }

                retrieveItems(0, StatisticalOperationsWebConstants.FORM_LIST_MAX_RESULTS, criteria);
                window.getFilter().getItemSchemeFilterFacet().getFormItem().getForm().markForRedraw();
            }
        });
        resourceTypeSelectorForm.setItems(resourceTypeItem);
        return resourceTypeSelectorForm;
    }

    protected void retrieveItemSchemes(int firstResult, int maxResults, SrmExternalResourceRestCriteria webCriteria) {
        getUiHandlers().retrieveConceptSchemes(getName(), webCriteria, firstResult, maxResults, getConceptSchemeTypes());
    }

    protected void retrieveItems(int firstResult, int maxResults, SrmItemRestCriteria webCriteria) {
        if (TypeExternalArtefactsEnum.CONCEPT_SCHEME.equals(getSelectedTypeExternalArtefactsEnum())) {
            SrmExternalResourceRestCriteria criteria = new SrmExternalResourceRestCriteria(TypeExternalArtefactsEnum.CONCEPT_SCHEME);
            criteria.setCriteria(window.getFilter().getSearchCriteria().getCriteria());
            criteria.setOnlyLastVersion(window.getFilter().getSearchCriteria().isItemSchemeLastVersion());
            retrieveItemSchemes(firstResult, maxResults, criteria);
        } else {
            getUiHandlers().retrieveConcepts(getName(), webCriteria, firstResult, maxResults, getConceptSchemeTypes());
        }
    }

    protected abstract ConceptSchemeTypeEnum[] getConceptSchemeTypes();

    public void setItemSchemes(ExternalItemsResult result) {
        if (TypeExternalArtefactsEnum.CONCEPT_SCHEME.equals(getSelectedTypeExternalArtefactsEnum())) {
            setResources(result.getExternalItemDtos(), result.getFirstResult(), result.getTotalResults());
        } else {
            setFilterResources(result.getExternalItemDtos(), result.getFirstResult(), result.getTotalResults());
        }
    }

    public void setItems(ExternalItemsResult result) {
        setResources(result.getExternalItemDtos(), result.getFirstResult(), result.getTotalResults());
    }

    private void setFilterResources(List<ExternalItemDto> externalItemsDtos, int firstResult, int totalResults) {
        if (window != null) {
            window.setFilterResources(externalItemsDtos);
            window.refreshFilterSourcePaginationInfo(firstResult, externalItemsDtos.size(), totalResults);
        }
    }

    private void setResources(List<ExternalItemDto> externalItemsDtos, int firstResult, int totalResults) {
        if (window != null) {
            window.setResources(externalItemsDtos);
            window.refreshSourcePaginationInfo(firstResult, externalItemsDtos.size(), totalResults);
        }
    }

    public void setUiHandlers(InstanceUiHandlers uiHandlers) {
        this.uiHandlers = uiHandlers;
    }

    public InstanceUiHandlers getUiHandlers() {
        return uiHandlers;
    }

    private LinkedHashMap<String, String> getConceptAndConceptSchemeValueMap() {
        LinkedHashMap<String, String> valueMap = new LinkedHashMap<String, String>();
        valueMap.put(TypeExternalArtefactsEnum.CONCEPT_SCHEME.name(), getConstants().conceptSchemes());
        valueMap.put(TypeExternalArtefactsEnum.CONCEPT.name(), getConstants().concepts());
        return valueMap;
    }

    private TypeExternalArtefactsEnum getSelectedTypeExternalArtefactsEnum() {
        if (resourceTypeSelectorForm.getItem(RESOURCE_TYPE_FIELD_NAME).getValue() != null) {
            String value = (String) resourceTypeSelectorForm.getItem(RESOURCE_TYPE_FIELD_NAME).getValue();
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
}
