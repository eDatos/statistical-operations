package org.siemac.metamac.statistical.operations.web.client.instance.view;

import static org.siemac.metamac.statistical.operations.web.client.OperationsWeb.getConstants;
import static org.siemac.metamac.statistical.operations.web.client.OperationsWeb.getMessages;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import org.siemac.metamac.core.common.dto.ExternalItemDto;
import org.siemac.metamac.core.common.dto.InternationalStringDto;
import org.siemac.metamac.core.common.util.shared.StringUtils;
import org.siemac.metamac.statistical.operations.core.dto.CollMethodDto;
import org.siemac.metamac.statistical.operations.core.dto.CostDto;
import org.siemac.metamac.statistical.operations.core.dto.InstanceDto;
import org.siemac.metamac.statistical.operations.core.dto.InstanceTypeDto;
import org.siemac.metamac.statistical.operations.core.dto.SurveySourceDto;
import org.siemac.metamac.statistical.operations.core.enume.domain.ProcStatusEnum;
import org.siemac.metamac.statistical.operations.web.client.enums.ToolStripButtonEnum;
import org.siemac.metamac.statistical.operations.web.client.instance.presenter.InstancePresenter;
import org.siemac.metamac.statistical.operations.web.client.instance.view.handlers.InstanceUiHandlers;
import org.siemac.metamac.statistical.operations.web.client.model.ds.InstanceDS;
import org.siemac.metamac.statistical.operations.web.client.utils.ClientSecurityUtils;
import org.siemac.metamac.statistical.operations.web.client.utils.CommonUtils;
import org.siemac.metamac.statistical.operations.web.client.utils.ConfigurationPropertiesUtils;
import org.siemac.metamac.statistical.operations.web.client.utils.OperationsListUtils;
import org.siemac.metamac.statistical.operations.web.client.utils.RequiredFieldUtils;
import org.siemac.metamac.statistical.operations.web.client.widgets.InstanceMainFormLayout;
import org.siemac.metamac.statistical.operations.web.client.widgets.external.SearchMultipleCodesItem;
import org.siemac.metamac.statistical.operations.web.client.widgets.external.SearchMultipleConceptsAndConceptSchemesForMeasuresItem;
import org.siemac.metamac.statistical.operations.web.client.widgets.external.SearchMultipleConceptsAndConceptSchemesForStatConcDefItem;
import org.siemac.metamac.statistical.operations.web.client.widgets.external.SearchMultipleConceptsForStatisticalUnitItem;
import org.siemac.metamac.statistical.operations.web.client.widgets.external.SearchMultipleDataProvidersItem;
import org.siemac.metamac.statistical.operations.web.client.widgets.external.SearchMultipleSrmItemSchemesItem;
import org.siemac.metamac.web.common.client.utils.CommonWebUtils;
import org.siemac.metamac.web.common.client.utils.CustomRequiredValidator;
import org.siemac.metamac.web.common.client.utils.FormItemUtils;
import org.siemac.metamac.web.common.client.utils.InternationalStringUtils;
import org.siemac.metamac.web.common.client.utils.RecordUtils;
import org.siemac.metamac.web.common.client.utils.TimeVariableWebUtils;
import org.siemac.metamac.web.common.client.widgets.InformationWindow;
import org.siemac.metamac.web.common.client.widgets.form.GroupDynamicForm;
import org.siemac.metamac.web.common.client.widgets.form.fields.CustomSelectItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.MultiLanguageTextItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.MultilanguageRichTextEditorItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.RequiredTextItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.ViewMultiLanguageTextItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.ViewTextItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.external.ExternalItemListItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.external.MultipleExternalResourceAction;
import org.siemac.metamac.web.common.client.widgets.form.fields.external.SearchMultipleSrmItemsItem;
import org.siemac.metamac.web.common.shared.domain.ExternalItemsResult;

import com.google.gwt.user.client.ui.Widget;
import com.gwtplatform.mvp.client.ViewWithUiHandlers;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.events.HasClickHandlers;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.FormItemIfFunction;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.FormItemClickHandler;
import com.smartgwt.client.widgets.form.fields.events.FormItemIconClickEvent;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

public class InstanceViewImpl extends ViewWithUiHandlers<InstanceUiHandlers> implements InstancePresenter.InstanceView {

    private VLayout                         panel;

    private InstanceMainFormLayout          mainFormLayout;

    // IDENTIFIERS
    private GroupDynamicForm                identifiersViewForm;
    private GroupDynamicForm                identifiersEditionForm;
    private RequiredTextItem                code;
    private MultiLanguageTextItem           title;
    private MultiLanguageTextItem           acronym;

    // CONTENT DESCRIPTORS
    private GroupDynamicForm                contentDescriptorsForm;
    private GroupDynamicForm                contentDescriptorsEditionForm;
    private MultilanguageRichTextEditorItem dataDescriptionItem;
    private MultilanguageRichTextEditorItem statisticalPopulationItem;
    private MultilanguageRichTextEditorItem geographicalComparabilityItem;
    private MultilanguageRichTextEditorItem temporalComparabilityItem;

    // CLASS DESCRIPTORS
    private GroupDynamicForm                classViewForm;
    private GroupDynamicForm                classDescriptorsEditionForm;
    private CustomSelectItem                instanceTypeItem;

    // PRODUCTION DESCRIPTORS
    private GroupDynamicForm                productionDescriptorsForm;
    private GroupDynamicForm                productionDescriptorsEditionForm;
    private MultilanguageRichTextEditorItem docMethodItem;
    private CustomSelectItem                collMethodItem;
    private CustomSelectItem                surveySourceItem;
    private MultilanguageRichTextEditorItem dataValidationItem;
    private MultilanguageRichTextEditorItem dataCompilationItem;
    private MultilanguageRichTextEditorItem adjustmentItem;
    private MultilanguageRichTextEditorItem costBurdenItem;
    private CustomSelectItem                costItem;

    // DIFFUSION AND PUBLICATION
    private GroupDynamicForm                diffusionViewForm;
    private GroupDynamicForm                diffusionEditionForm;

    // QUALITY DESCRIPTORS
    private GroupDynamicForm                qualityViewForm;
    private GroupDynamicForm                qualityEditionForm;
    private MultilanguageRichTextEditorItem qualityDocItem;
    private MultilanguageRichTextEditorItem qualityAssureItem;
    private MultilanguageRichTextEditorItem qualityAssesmentItem;
    private MultilanguageRichTextEditorItem userNeedsItem;
    private MultilanguageRichTextEditorItem userSatItem;
    private MultilanguageRichTextEditorItem completenessItem;
    private MultilanguageRichTextEditorItem timelinessItem;
    private MultilanguageRichTextEditorItem punctualityItem;
    private MultilanguageRichTextEditorItem accuracyOverallItem;
    private MultilanguageRichTextEditorItem samplingErrItem;
    private MultilanguageRichTextEditorItem nonSamplingErrItem;
    private MultilanguageRichTextEditorItem coherXDomItem;
    private MultilanguageRichTextEditorItem coherInternalItem;

    // ANNOTATIONS
    private GroupDynamicForm                annotationsViewForm;
    private GroupDynamicForm                annotationsEditionForm;
    private MultilanguageRichTextEditorItem commentItem;
    private MultilanguageRichTextEditorItem notesItem;

    private List<InstanceTypeDto>           instanceTypeDtos;
    private List<SurveySourceDto>           surveySourceDtos;
    private List<CollMethodDto>             collMethodDtos;
    private List<CostDto>                   costDtos;

    private InstanceDto                     instanceDto;
    public String                           operationCode;

    public InstanceViewImpl() {
        super();
        panel = new VLayout();

        // Instance

        mainFormLayout = new InstanceMainFormLayout();
        mainFormLayout.getTranslateToolStripButton().addClickHandler(new ClickHandler() {

            @Override
            public void onClick(ClickEvent event) {
                setTranslationsShowed(mainFormLayout.getTranslateToolStripButton().isSelected());
            }
        });
        mainFormLayout.getDeleteConfirmationWindow().getYesButton().addClickHandler(new ClickHandler() {

            @Override
            public void onClick(ClickEvent event) {
                getUiHandlers().deleteInstance(instanceDto);
            }
        });
        createViewForm();
        createEditionForm();

        VLayout subPanel = new VLayout();
        subPanel.setHeight100();
        subPanel.setOverflow(Overflow.SCROLL);
        subPanel.addMember(mainFormLayout);

        panel.addMember(subPanel);
    }

    @Override
    public Widget asWidget() {
        return panel;
    }

    @Override
    public void setUiHandlers(InstanceUiHandlers uiHandlers) {
        super.setUiHandlers(uiHandlers);

        // Set uiHandlers in formItems

        ((SearchMultipleSrmItemsItem) contentDescriptorsEditionForm.getItem(InstanceDS.STATISTICAL_UNIT)).setUiHandlers(uiHandlers);
        ((SearchMultipleSrmItemSchemesItem) contentDescriptorsEditionForm.getItem(InstanceDS.CLASS_SYSTEM_LIST)).setUiHandlers(uiHandlers);
        ((SearchMultipleSrmItemsItem) contentDescriptorsEditionForm.getItem(InstanceDS.GEOGRAPHIC_GRANULARITIES)).setUiHandlers(uiHandlers);
        ((SearchMultipleSrmItemsItem) contentDescriptorsEditionForm.getItem(InstanceDS.TEMPORAL_GRANULARITIES)).setUiHandlers(uiHandlers);
        ((SearchMultipleSrmItemsItem) contentDescriptorsEditionForm.getItem(InstanceDS.STAT_CONC_DEF)).setUiHandlers(uiHandlers);
        ((SearchMultipleSrmItemsItem) contentDescriptorsEditionForm.getItem(InstanceDS.MEASURES)).setUiHandlers(uiHandlers);

        ((SearchMultipleSrmItemsItem) productionDescriptorsEditionForm.getItem(InstanceDS.INFORMATION_SUPPLIERS)).setUiHandlers(uiHandlers);
        ((SearchMultipleSrmItemsItem) productionDescriptorsEditionForm.getItem(InstanceDS.FREQ_COLL)).setUiHandlers(uiHandlers);
    }

    /*
     * GWTP will call setInSlot when a child presenter asks to be added under this view
     */
    @Override
    public void setInSlot(Object slot, Widget content) {
        if (slot == InstancePresenter.TYPE_SetContextAreaContentToolBar) {
            if (content != null) {
                Canvas[] canvas = ((ToolStrip) content).getMembers();
                for (int i = 0; i < canvas.length; i++) {
                    if (canvas[i] instanceof ToolStripButton) {
                        if (ToolStripButtonEnum.OPERATIONS.getValue().equals(((ToolStripButton) canvas[i]).getID())) {
                            ((ToolStripButton) canvas[i]).select();
                        }
                    }
                }
                panel.addMember(content, 0);
            }
        } else {
            // To support inheritance in your views it is good practice to call super.setInSlot when you can't handle the call.
            // Who knows, maybe the parent class knows what to do with this slot.
            super.setInSlot(slot, content);
        }
    }

    @Override
    public void setInstance(InstanceDto instanceDto, String operationCode) {
        this.instanceDto = instanceDto;
        this.operationCode = operationCode;

        // Security
        mainFormLayout.setCanEdit(ClientSecurityUtils.canUpdateInstance(operationCode));
        mainFormLayout.setCanDelete(ClientSecurityUtils.canDeleteInstance(operationCode, instanceDto.getProcStatus()));
        mainFormLayout.setOperationCode(operationCode);

        mainFormLayout.setViewMode();
        mainFormLayout.updatePublishSection(instanceDto.getProcStatus());
        // Set Instance
        mainFormLayout.setTitleLabelContents(InternationalStringUtils.getLocalisedString(instanceDto.getTitle()));
        setInstanceViewMode(instanceDto);
        setInstanceEditionMode(instanceDto);
    }

    @Override
    public HasClickHandlers getSave() {
        return mainFormLayout.getSave();
    }

    @Override
    public void onInstanceSaved(InstanceDto instanceDto) {
        setInstance(instanceDto, this.operationCode); // Operation code should be the same
    }

    @Override
    public InstanceDto getInstance(InstanceDto instanceDto) {

        // IDENTIFIERS

        instanceDto.setCode(code.getValueAsString());
        instanceDto.setTitle(title.getValue());
        instanceDto.setAcronym(acronym.getValue());

        // CONTENT CLASSIFIERS

        // CONTENT DESCRIPTORS

        instanceDto.setDataDescription(dataDescriptionItem.getValue());
        instanceDto.setStatisticalPopulation(statisticalPopulationItem.getValue());

        List<ExternalItemDto> statisticalUnits = ((ExternalItemListItem) contentDescriptorsEditionForm.getItem(InstanceDS.STATISTICAL_UNIT)).getExternalItemDtos();
        instanceDto.getStatisticalUnit().clear();
        instanceDto.getStatisticalUnit().addAll(statisticalUnits);

        List<ExternalItemDto> geographicGranularities = ((ExternalItemListItem) contentDescriptorsEditionForm.getItem(InstanceDS.GEOGRAPHIC_GRANULARITIES)).getExternalItemDtos();
        instanceDto.getGeographicGranularity().clear();
        instanceDto.getGeographicGranularity().addAll(geographicGranularities);

        instanceDto.setGeographicComparability(geographicalComparabilityItem.getValue());

        List<ExternalItemDto> temporalGranularities = ((ExternalItemListItem) contentDescriptorsEditionForm.getItem(InstanceDS.TEMPORAL_GRANULARITIES)).getExternalItemDtos();
        instanceDto.getTemporalGranularity().clear();
        instanceDto.getTemporalGranularity().addAll(temporalGranularities);

        instanceDto.setTemporalComparability(temporalComparabilityItem.getValue());
        instanceDto.setBasePeriod(contentDescriptorsEditionForm.getValueAsString(InstanceDS.BASE_PERIOD));

        List<ExternalItemDto> measures = ((ExternalItemListItem) contentDescriptorsEditionForm.getItem(InstanceDS.MEASURES)).getExternalItemDtos();
        instanceDto.getUnitMeasure().clear();
        instanceDto.getUnitMeasure().addAll(measures);

        instanceDto.setStatConcDef((InternationalStringDto) contentDescriptorsEditionForm.getValue(InstanceDS.STAT_CONC_DEF_DESCRIPTION));

        List<ExternalItemDto> statConcDef = ((ExternalItemListItem) contentDescriptorsEditionForm.getItem(InstanceDS.STAT_CONC_DEF)).getExternalItemDtos();
        instanceDto.getStatConcDefList().clear();
        instanceDto.getStatConcDefList().addAll(statConcDef);

        instanceDto.setClassSystem((InternationalStringDto) contentDescriptorsEditionForm.getValue(InstanceDS.CLASS_SYSTEM_DESCRIPTION));

        List<ExternalItemDto> classSystems = ((ExternalItemListItem) contentDescriptorsEditionForm.getItem(InstanceDS.CLASS_SYSTEM_LIST)).getExternalItemDtos();
        instanceDto.getClassSystemList().clear();
        instanceDto.getClassSystemList().addAll(classSystems);

        // CLASS DESCRIPTORS

        instanceDto.setInstanceType(OperationsListUtils.getInstanceTypeDto(instanceTypeItem.getValueAsString(), instanceTypeDtos));

        // PRODUCTION DESCRIPTORS

        instanceDto.setDocMethod(docMethodItem.getValue());
        instanceDto.setSurveySource(OperationsListUtils.getSurveySourceDto(surveySourceItem.getValueAsString(), surveySourceDtos));
        instanceDto.setCollMethod(OperationsListUtils.getCollMethodDto(collMethodItem.getValueAsString(), collMethodDtos));

        List<ExternalItemDto> informationSuppliers = ((ExternalItemListItem) productionDescriptorsEditionForm.getItem(InstanceDS.INFORMATION_SUPPLIERS)).getExternalItemDtos();
        instanceDto.getInformationSuppliers().clear();
        instanceDto.getInformationSuppliers().addAll(informationSuppliers);

        List<ExternalItemDto> freqColls = ((ExternalItemListItem) productionDescriptorsEditionForm.getItem(InstanceDS.FREQ_COLL)).getExternalItemDtos();
        instanceDto.getFreqColl().clear();
        instanceDto.getFreqColl().addAll(freqColls);

        instanceDto.setDataValidation(dataValidationItem.getValue());
        instanceDto.setDataCompilation(dataCompilationItem.getValue());
        instanceDto.setAdjustment(adjustmentItem.getValue());
        instanceDto.setCostBurden(costBurdenItem.getValue());
        instanceDto.getCost().clear();
        instanceDto.getCost().addAll(OperationsListUtils.getCostDtos(costItem.getValues(), costDtos));

        // QUALITY DESCRIPTORS

        instanceDto.setQualityDoc(qualityDocItem.getValue());
        instanceDto.setQualityAssure(qualityAssureItem.getValue());
        instanceDto.setQualityAssmnt(qualityAssesmentItem.getValue());
        instanceDto.setUserNeeds(userNeedsItem.getValue());
        instanceDto.setUserSat(userSatItem.getValue());
        instanceDto.setCompleteness(completenessItem.getValue());
        instanceDto.setTimeliness(timelinessItem.getValue());
        instanceDto.setPunctuality(punctualityItem.getValue());
        instanceDto.setAccuracyOverall(accuracyOverallItem.getValue());
        instanceDto.setSamplingErr(samplingErrItem.getValue());
        instanceDto.setNonsamplingErr(nonSamplingErrItem.getValue());
        instanceDto.setCoherXDomain(coherXDomItem.getValue());
        instanceDto.setCoherInternal(coherInternalItem.getValue());

        // ANNOTATIONS

        instanceDto.setComment(commentItem.getValue());
        instanceDto.setNotes(notesItem.getValue());

        return instanceDto;
    }

    @Override
    public boolean validate() {
        return identifiersEditionForm.validate(false) &&

        contentDescriptorsEditionForm.validate(false) && classDescriptorsEditionForm.validate(false) && productionDescriptorsEditionForm.validate(false) && diffusionEditionForm.validate(false)
                && qualityEditionForm.validate(false) && annotationsEditionForm.validate(false);
    }

    private void createViewForm() {
        // Identifiers
        identifiersViewForm = new GroupDynamicForm(getConstants().operationIdentifiers());
        ViewTextItem identifier = new ViewTextItem(InstanceDS.CODE, getConstants().instanceCode());
        ViewMultiLanguageTextItem title = new ViewMultiLanguageTextItem(InstanceDS.TITLE, getConstants().instanceTitle());
        ViewMultiLanguageTextItem alternativeTitle = new ViewMultiLanguageTextItem(InstanceDS.ACRONYM, getConstants().instanceAcronym());
        ViewTextItem urn = new ViewTextItem(InstanceDS.URN, getConstants().instanceUrn());
        identifiersViewForm.setFields(identifier, title, alternativeTitle, urn);

        // Content classifiers

        // Content descriptors
        contentDescriptorsForm = new GroupDynamicForm(getConstants().instanceContentDescriptors());
        ViewMultiLanguageTextItem dataDescription = new ViewMultiLanguageTextItem(InstanceDS.DATA_DESCRIPTION, getConstants().instanceDataDescription());
        ViewMultiLanguageTextItem statisticalPopulation = new ViewMultiLanguageTextItem(InstanceDS.STATISTICAL_POPULATION, getConstants().instanceStatisticalPopulation());
        ExternalItemListItem statisticalUnit = new ExternalItemListItem(InstanceDS.STATISTICAL_UNIT, getConstants().instanceStatisticalUnit(), false);
        ExternalItemListItem geographicGranularities = new ExternalItemListItem(InstanceDS.GEOGRAPHIC_GRANULARITIES, getConstants().instanceGeographicGranularity(), false);
        ViewMultiLanguageTextItem geographicComparability = new ViewMultiLanguageTextItem(InstanceDS.GEOGRAPHIC_COMPARABILITY, getConstants().instanceGeographicComparability());
        ExternalItemListItem temporalGranularities = new ExternalItemListItem(InstanceDS.TEMPORAL_GRANULARITIES, getConstants().instanceTemporalGranularity(), false);
        ViewMultiLanguageTextItem temporalComparability = new ViewMultiLanguageTextItem(InstanceDS.TEMPORAL_COMPARABILITY, getConstants().instanceTemporalComparability());
        ViewTextItem basePeriodItem = new ViewTextItem(InstanceDS.BASE_PERIOD, getConstants().instanceBasePeriod());
        ExternalItemListItem measures = new ExternalItemListItem(InstanceDS.MEASURES, getConstants().instanceUnitMeasure(), false);
        ViewMultiLanguageTextItem statConcDefDescription = new ViewMultiLanguageTextItem(InstanceDS.STAT_CONC_DEF_DESCRIPTION, getConstants().instanceStatisticalConceptDefinition());
        ExternalItemListItem statConcDefList = new ExternalItemListItem(InstanceDS.STAT_CONC_DEF, getConstants().instanceStatisticalConceptsDefinitions(), false);
        ExternalItemListItem classSystemList = new ExternalItemListItem(InstanceDS.CLASS_SYSTEM_LIST, getConstants().instanceClassSystemList(), false);
        ViewMultiLanguageTextItem classSystemDescription = new ViewMultiLanguageTextItem(InstanceDS.CLASS_SYSTEM_DESCRIPTION, getConstants().instanceClassSystem());
        contentDescriptorsForm.setFields(dataDescription, statisticalPopulation, statisticalUnit, basePeriodItem, geographicGranularities, temporalGranularities, geographicComparability,
                temporalComparability, measures, statConcDefDescription, statConcDefList, classSystemList, classSystemDescription);

        // Class descriptors
        classViewForm = new GroupDynamicForm(getConstants().instanceClassDescriptors());
        ViewTextItem instanceType = new ViewTextItem(InstanceDS.INSTANCE_TYPE, getConstants().instanceType());
        classViewForm.setFields(instanceType);

        // Production descriptors
        productionDescriptorsForm = new GroupDynamicForm(getConstants().instanceProductionDescriptors());
        ViewTextItem createdDate = new ViewTextItem(InstanceDS.CREATED_DATE, getConstants().instanceCreatedDate());
        ViewTextItem internalInventoryDate = new ViewTextItem(InstanceDS.INTERNAL_INVENTORY_DATE, getConstants().instanceInternalInventoryDate());
        ViewTextItem procStatus = new ViewTextItem(InstanceDS.PROC_STATUS, getConstants().instanceProcStatus());
        ViewMultiLanguageTextItem staticDocMethodItem = new ViewMultiLanguageTextItem(InstanceDS.DOC_METHOD, getConstants().instanceDocMethod());
        ViewTextItem surveySource = new ViewTextItem(InstanceDS.STATISTICAL_OPERATION_SOURCE, getConstants().instanceStatisticalOperationSource());
        ViewTextItem collMethod = new ViewTextItem(InstanceDS.COLL_METHOD, getConstants().instanceCollMethod());
        ExternalItemListItem informationSuppliers = new ExternalItemListItem(InstanceDS.INFORMATION_SUPPLIERS, getConstants().instanceInformationSuppliers(), false);
        ExternalItemListItem freqColl = new ExternalItemListItem(InstanceDS.FREQ_COLL, getConstants().instanceFreqColl(), false);
        ViewMultiLanguageTextItem staticDataValidationItem = new ViewMultiLanguageTextItem(InstanceDS.DATA_VALIDATION, getConstants().instanceDataValidation());
        ViewMultiLanguageTextItem staticDataCompilationItem = new ViewMultiLanguageTextItem(InstanceDS.DATA_COMPILATION, getConstants().instanceDataCompilation());
        ViewMultiLanguageTextItem staticAdjustmentItem = new ViewMultiLanguageTextItem(InstanceDS.ADJUSTMENT, getConstants().instanceAdjustment());
        ViewMultiLanguageTextItem staticCostBurdenItem = new ViewMultiLanguageTextItem(InstanceDS.COST_BURDEN, getConstants().instanceCostBurden());
        ViewTextItem cost = new ViewTextItem(InstanceDS.COST, getConstants().instanceCost());
        productionDescriptorsForm.setFields(createdDate, internalInventoryDate, procStatus, staticDocMethodItem, surveySource, collMethod, informationSuppliers, freqColl, staticDataValidationItem,
                staticDataCompilationItem, staticAdjustmentItem, staticCostBurdenItem, cost);

        // Diffusion and Publication
        diffusionViewForm = new GroupDynamicForm(getConstants().instanceDiffusionDescriptors());
        ViewTextItem inventoryDate = new ViewTextItem(InstanceDS.INVENTORY_DATE, getConstants().instanceInventoryDate());
        diffusionViewForm.setFields(inventoryDate);

        // Quality descriptors
        qualityViewForm = new GroupDynamicForm(getConstants().instanceQualityDescriptors());
        ViewMultiLanguageTextItem staticQualityDocItem = new ViewMultiLanguageTextItem(InstanceDS.QUALITY_DOC, getConstants().instanceQDoc());
        ViewMultiLanguageTextItem staticQualityAssureItem = new ViewMultiLanguageTextItem(InstanceDS.QUALITY_ASSURE, getConstants().instanceQAssure());
        ViewMultiLanguageTextItem staticQualityAssesmentItem = new ViewMultiLanguageTextItem(InstanceDS.QUALITY_ASSMNT, getConstants().instanceQAssmnt());
        ViewMultiLanguageTextItem staticUserNeedsItem = new ViewMultiLanguageTextItem(InstanceDS.USER_NEEDS, getConstants().instanceUserNeeds());
        ViewMultiLanguageTextItem staticUserSatItem = new ViewMultiLanguageTextItem(InstanceDS.USER_SAT, getConstants().instanceUserSat());
        ViewMultiLanguageTextItem staticCompletenessItem = new ViewMultiLanguageTextItem(InstanceDS.COMPLETENESS, getConstants().instanceCompleteness());
        ViewMultiLanguageTextItem staticTimelinessItem = new ViewMultiLanguageTextItem(InstanceDS.TIMELINESS, getConstants().instanceTimeliness());
        ViewMultiLanguageTextItem staticPunctualityItem = new ViewMultiLanguageTextItem(InstanceDS.PUNCTUALITY, getConstants().instancePunctuality());
        ViewMultiLanguageTextItem staticAccuracyOverallItem = new ViewMultiLanguageTextItem(InstanceDS.ACCURACY_OVERALL, getConstants().instanceAccuracyOverall());
        ViewMultiLanguageTextItem staticSamplingErrItem = new ViewMultiLanguageTextItem(InstanceDS.SAMPLING_ERROR, getConstants().instanceSamplingErr());
        ViewMultiLanguageTextItem statocNonSamplingErrItem = new ViewMultiLanguageTextItem(InstanceDS.NONSAMPLING_ERR, getConstants().instanceNonSamplingErr());
        ViewMultiLanguageTextItem staticCoherXDomItem = new ViewMultiLanguageTextItem(InstanceDS.COHER_X_DOM, getConstants().instanceCoherXDom());
        ViewMultiLanguageTextItem staticCoherInternalItem = new ViewMultiLanguageTextItem(InstanceDS.COHER_INTERNAL, getConstants().instanceCoherInter());
        qualityViewForm.setFields(staticQualityDocItem, staticQualityAssureItem, staticQualityAssesmentItem, staticUserNeedsItem, staticUserSatItem, staticCompletenessItem, staticTimelinessItem,
                staticPunctualityItem, staticAccuracyOverallItem, staticSamplingErrItem, statocNonSamplingErrItem, staticCoherXDomItem, staticCoherInternalItem);

        // Annotations
        annotationsViewForm = new GroupDynamicForm(getConstants().instanceAnnotations());
        ViewMultiLanguageTextItem staticCommentItem = new ViewMultiLanguageTextItem(InstanceDS.COMMENTS, getConstants().instanceComments());
        ViewMultiLanguageTextItem staticNotesItem = new ViewMultiLanguageTextItem(InstanceDS.NOTES, getConstants().instanceNotes());
        annotationsViewForm.setFields(staticCommentItem, staticNotesItem);

        mainFormLayout.addViewCanvas(identifiersViewForm);

        mainFormLayout.addViewCanvas(contentDescriptorsForm);
        mainFormLayout.addViewCanvas(classViewForm);
        mainFormLayout.addViewCanvas(productionDescriptorsForm);
        mainFormLayout.addViewCanvas(diffusionViewForm);
        mainFormLayout.addViewCanvas(qualityViewForm);
        mainFormLayout.addViewCanvas(annotationsViewForm);
    }

    private void createEditionForm() {
        // Identifiers
        identifiersEditionForm = new GroupDynamicForm(getConstants().instanceIdentifiers());

        code = new RequiredTextItem(InstanceDS.CODE, getConstants().instanceCode());
        code.setShowIfCondition(new FormItemIfFunction() {

            @Override
            public boolean execute(FormItem item, Object value, DynamicForm form) {
                return canInstanceCodeBeEdited();
            }
        });
        code.setValidators(CommonWebUtils.getSemanticIdentifierCustomValidator());
        ViewTextItem staticCode = new ViewTextItem(InstanceDS.CODE_VIEW, getConstants().instanceCode());
        staticCode.setShowIfCondition(new FormItemIfFunction() {

            @Override
            public boolean execute(FormItem item, Object value, DynamicForm form) {
                return !canInstanceCodeBeEdited();
            }
        });

        title = new MultiLanguageTextItem(InstanceDS.TITLE, getConstants().instanceTitle());
        title.setRequired(true);
        acronym = new MultiLanguageTextItem(InstanceDS.ACRONYM, getConstants().instanceAcronym());
        ViewTextItem urn = new ViewTextItem(InstanceDS.URN, getConstants().instanceUrn());
        identifiersEditionForm.setFields(staticCode, code, title, acronym, urn);

        // Content classifiers

        // Content descriptors
        contentDescriptorsEditionForm = new GroupDynamicForm(getConstants().instanceContentDescriptors());
        dataDescriptionItem = new MultilanguageRichTextEditorItem(InstanceDS.DATA_DESCRIPTION, getConstants().instanceDataDescription());
        statisticalPopulationItem = new MultilanguageRichTextEditorItem(InstanceDS.STATISTICAL_POPULATION, getConstants().instanceStatisticalPopulation());
        ExternalItemListItem statisticalUnitItem = createStatisticalUnitsItem(InstanceDS.STATISTICAL_UNIT, getConstants().instanceStatisticalUnit());
        ExternalItemListItem geographicGranularities = createGeographicGranularities(InstanceDS.GEOGRAPHIC_GRANULARITIES, getConstants().instanceGeographicGranularity());
        geographicalComparabilityItem = new MultilanguageRichTextEditorItem(InstanceDS.GEOGRAPHIC_COMPARABILITY, getConstants().instanceGeographicComparability());
        ExternalItemListItem temporalGranularities = createTemporalGranularities(InstanceDS.TEMPORAL_GRANULARITIES, getConstants().instanceTemporalGranularity());
        temporalComparabilityItem = new MultilanguageRichTextEditorItem(InstanceDS.TEMPORAL_COMPARABILITY, getConstants().instanceTemporalComparability());
        TextItem basePeriodItem = new TextItem(InstanceDS.BASE_PERIOD, getConstants().instanceBasePeriod());
        basePeriodItem.setValidators(TimeVariableWebUtils.getTimeCustomValidator());

        ExternalItemListItem measuresItem = createMeasures(InstanceDS.MEASURES, getConstants().instanceUnitMeasure());

        MultilanguageRichTextEditorItem statConcDefDescriptionItem = new MultilanguageRichTextEditorItem(InstanceDS.STAT_CONC_DEF_DESCRIPTION, getConstants().instanceStatisticalConceptDefinition());

        ExternalItemListItem statConcDefItem = createStatConcDef(InstanceDS.STAT_CONC_DEF, getConstants().instanceStatisticalConceptsDefinitions());

        ExternalItemListItem classSystemItem = createClassSystemItem(InstanceDS.CLASS_SYSTEM_LIST, getConstants().instanceClassSystemList());

        MultilanguageRichTextEditorItem classSystemDescriptionItem = new MultilanguageRichTextEditorItem(InstanceDS.CLASS_SYSTEM_DESCRIPTION, getConstants().instanceClassSystem());

        contentDescriptorsEditionForm.setFields(dataDescriptionItem, statisticalPopulationItem, statisticalUnitItem, basePeriodItem, geographicGranularities, temporalGranularities,
                geographicalComparabilityItem, temporalComparabilityItem, measuresItem, statConcDefDescriptionItem, statConcDefItem, classSystemItem, classSystemDescriptionItem);

        // Class descriptors
        classDescriptorsEditionForm = new GroupDynamicForm(getConstants().instanceClassDescriptors());

        instanceTypeItem = new CustomSelectItem(InstanceDS.INSTANCE_TYPE, getConstants().instanceType());
        instanceTypeItem.setValidators(new CustomRequiredValidator() {

            @Override
            protected boolean condition(Object value) {
                return CommonUtils.isInternallyOrExternallyPublished(instanceDto) ? !StringUtils.isBlank(instanceTypeItem.getValueAsString()) : true;
            }
        });

        classDescriptorsEditionForm.setFields(instanceTypeItem);

        // Production descriptors
        productionDescriptorsEditionForm = new GroupDynamicForm(getConstants().instanceProductionDescriptors());
        ViewTextItem createdDate = new ViewTextItem(InstanceDS.CREATED_DATE, getConstants().instanceCreatedDate());
        ViewTextItem internalInventoryDate = new ViewTextItem(InstanceDS.INTERNAL_INVENTORY_DATE, getConstants().instanceInternalInventoryDate());

        ViewTextItem procStatus = new ViewTextItem(InstanceDS.PROC_STATUS, getConstants().instanceProcStatus());
        ViewTextItem staticProcStatus = new ViewTextItem(InstanceDS.PROC_STATUS_VIEW, getConstants().instanceProcStatus());
        staticProcStatus.setShowIfCondition(FormItemUtils.getFalseFormItemIfFunction());

        docMethodItem = new MultilanguageRichTextEditorItem(InstanceDS.DOC_METHOD, getConstants().instanceDocMethod());
        surveySourceItem = new CustomSelectItem(InstanceDS.STATISTICAL_OPERATION_SOURCE, getConstants().instanceStatisticalOperationSource());
        collMethodItem = new CustomSelectItem(InstanceDS.COLL_METHOD, getConstants().instanceCollMethod());
        SearchMultipleSrmItemsItem informationSuppliersItem = createInformationSuppliersItem(InstanceDS.INFORMATION_SUPPLIERS, getConstants().instanceInformationSuppliers());
        ExternalItemListItem freqCollItem = createFreqColl(InstanceDS.FREQ_COLL, getConstants().instanceFreqColl());
        dataValidationItem = new MultilanguageRichTextEditorItem(InstanceDS.DATA_VALIDATION, getConstants().instanceDataValidation());
        dataCompilationItem = new MultilanguageRichTextEditorItem(InstanceDS.DATA_COMPILATION, getConstants().instanceDataCompilation());
        adjustmentItem = new MultilanguageRichTextEditorItem(InstanceDS.ADJUSTMENT, getConstants().instanceAdjustment());
        costBurdenItem = new MultilanguageRichTextEditorItem(InstanceDS.COST_BURDEN, getConstants().instanceCostBurden());
        costItem = new CustomSelectItem(InstanceDS.COST, getConstants().instanceCost());
        costItem.setMultiple(true);
        productionDescriptorsEditionForm.setFields(createdDate, internalInventoryDate, staticProcStatus, procStatus, docMethodItem, surveySourceItem, collMethodItem, informationSuppliersItem,
                freqCollItem, dataValidationItem, dataCompilationItem, adjustmentItem, costBurdenItem, costItem);

        // Diffusion and Publication
        diffusionEditionForm = new GroupDynamicForm(getConstants().instanceDiffusionDescriptors());
        ViewTextItem inventoryDate = new ViewTextItem(InstanceDS.INVENTORY_DATE, getConstants().instanceInventoryDate());
        diffusionEditionForm.setFields(inventoryDate);

        // Quality Descriptors
        qualityEditionForm = new GroupDynamicForm(getConstants().instanceQualityDescriptors());
        qualityDocItem = new MultilanguageRichTextEditorItem(InstanceDS.QUALITY_DOC, getConstants().instanceQDoc());
        qualityAssureItem = new MultilanguageRichTextEditorItem(InstanceDS.QUALITY_ASSURE, getConstants().instanceQAssure());
        qualityAssesmentItem = new MultilanguageRichTextEditorItem(InstanceDS.QUALITY_ASSMNT, getConstants().instanceQAssmnt());
        userNeedsItem = new MultilanguageRichTextEditorItem(InstanceDS.USER_NEEDS, getConstants().instanceUserNeeds());
        userSatItem = new MultilanguageRichTextEditorItem(InstanceDS.USER_SAT, getConstants().instanceUserSat());
        completenessItem = new MultilanguageRichTextEditorItem(InstanceDS.COMPLETENESS, getConstants().instanceCompleteness());
        timelinessItem = new MultilanguageRichTextEditorItem(InstanceDS.TIMELINESS, getConstants().instanceTimeliness());
        punctualityItem = new MultilanguageRichTextEditorItem(InstanceDS.PUNCTUALITY, getConstants().instancePunctuality());
        accuracyOverallItem = new MultilanguageRichTextEditorItem(InstanceDS.ACCURACY_OVERALL, getConstants().instanceAccuracyOverall());
        samplingErrItem = new MultilanguageRichTextEditorItem(InstanceDS.SAMPLING_ERROR, getConstants().instanceSamplingErr());
        nonSamplingErrItem = new MultilanguageRichTextEditorItem(InstanceDS.NONSAMPLING_ERR, getConstants().instanceNonSamplingErr());
        coherXDomItem = new MultilanguageRichTextEditorItem(InstanceDS.COHER_X_DOM, getConstants().instanceCoherXDom());
        coherInternalItem = new MultilanguageRichTextEditorItem(InstanceDS.COHER_INTERNAL, getConstants().instanceCoherInter());
        qualityEditionForm.setFields(qualityDocItem, qualityAssureItem, qualityAssesmentItem, userNeedsItem, userSatItem, completenessItem, timelinessItem, punctualityItem, accuracyOverallItem,
                samplingErrItem, nonSamplingErrItem, coherXDomItem, coherInternalItem);

        // Annotations
        annotationsEditionForm = new GroupDynamicForm(getConstants().instanceAnnotations());
        commentItem = new MultilanguageRichTextEditorItem(InstanceDS.COMMENTS, getConstants().instanceComments());
        notesItem = new MultilanguageRichTextEditorItem(InstanceDS.NOTES, getConstants().instanceNotes());
        annotationsEditionForm.setFields(commentItem, notesItem);

        mainFormLayout.addEditionCanvas(identifiersEditionForm);

        mainFormLayout.addEditionCanvas(contentDescriptorsEditionForm);
        mainFormLayout.addEditionCanvas(classDescriptorsEditionForm);
        mainFormLayout.addEditionCanvas(productionDescriptorsEditionForm);
        mainFormLayout.addEditionCanvas(diffusionEditionForm);
        mainFormLayout.addEditionCanvas(qualityEditionForm);
        mainFormLayout.addEditionCanvas(annotationsEditionForm);
    }

    private void setInstanceViewMode(InstanceDto instanceDto) {

        // IDENTIFIERS

        identifiersViewForm.setValue(InstanceDS.CODE, instanceDto.getCode());
        identifiersViewForm.setValue(InstanceDS.TITLE, RecordUtils.getInternationalStringRecord(instanceDto.getTitle()));
        identifiersViewForm.setValue(InstanceDS.ACRONYM, RecordUtils.getInternationalStringRecord(instanceDto.getAcronym()));
        identifiersViewForm.setValue(InstanceDS.URN, instanceDto.getUrn());

        // CONTENT CLASSIFIERS

        // CONTENT DESCRIPTORS

        contentDescriptorsForm.setValue(InstanceDS.DATA_DESCRIPTION, RecordUtils.getInternationalStringRecord(instanceDto.getDataDescription()));
        contentDescriptorsForm.setValue(InstanceDS.STATISTICAL_POPULATION, RecordUtils.getInternationalStringRecord(instanceDto.getStatisticalPopulation()));

        ((ExternalItemListItem) contentDescriptorsForm.getItem(InstanceDS.STATISTICAL_UNIT)).setExternalItems(instanceDto.getStatisticalUnit());

        ((ExternalItemListItem) contentDescriptorsForm.getItem(InstanceDS.GEOGRAPHIC_GRANULARITIES)).setExternalItems(instanceDto.getGeographicGranularity());

        contentDescriptorsForm.setValue(InstanceDS.GEOGRAPHIC_COMPARABILITY, RecordUtils.getInternationalStringRecord(instanceDto.getGeographicComparability()));

        ((ExternalItemListItem) contentDescriptorsForm.getItem(InstanceDS.TEMPORAL_GRANULARITIES)).setExternalItems(instanceDto.getTemporalGranularity());

        contentDescriptorsForm.setValue(InstanceDS.TEMPORAL_COMPARABILITY, RecordUtils.getInternationalStringRecord(instanceDto.getTemporalComparability()));
        contentDescriptorsForm.setValue(InstanceDS.BASE_PERIOD, instanceDto.getBasePeriod());

        ((ExternalItemListItem) contentDescriptorsForm.getItem(InstanceDS.MEASURES)).setExternalItems(instanceDto.getUnitMeasure());

        contentDescriptorsForm.setValue(InstanceDS.STAT_CONC_DEF_DESCRIPTION, RecordUtils.getInternationalStringRecord(instanceDto.getStatConcDef()));

        ((ExternalItemListItem) contentDescriptorsForm.getItem(InstanceDS.STAT_CONC_DEF)).setExternalItems(instanceDto.getStatConcDefList());

        contentDescriptorsForm.setValue(InstanceDS.CLASS_SYSTEM_DESCRIPTION, RecordUtils.getInternationalStringRecord(instanceDto.getClassSystem()));
        ((ExternalItemListItem) contentDescriptorsForm.getItem(InstanceDS.CLASS_SYSTEM_LIST)).setExternalItems(instanceDto.getClassSystemList());

        // CLASS DESCRIPTORS

        classViewForm.setValue(InstanceDS.INSTANCE_TYPE,
                instanceDto.getInstanceType() != null ? CommonWebUtils.getElementName(instanceDto.getInstanceType().getIdentifier(), instanceDto.getInstanceType().getDescription()) : "");

        // PRODUCTION DESCRIPTORS

        productionDescriptorsForm.setValue(InstanceDS.CREATED_DATE, instanceDto.getCreatedDate());
        productionDescriptorsForm.setValue(InstanceDS.INTERNAL_INVENTORY_DATE, instanceDto.getInternalInventoryDate());
        productionDescriptorsForm.setValue(InstanceDS.PROC_STATUS, CommonUtils.getProcStatusName(instanceDto.getProcStatus()));

        productionDescriptorsForm.setValue(InstanceDS.DOC_METHOD, RecordUtils.getInternationalStringRecord(instanceDto.getDocMethod()));
        productionDescriptorsForm.setValue(InstanceDS.STATISTICAL_OPERATION_SOURCE,
                instanceDto.getSurveySource() != null ? CommonWebUtils.getElementName(instanceDto.getSurveySource().getIdentifier(), instanceDto.getSurveySource().getDescription()) : "");
        productionDescriptorsForm.setValue(InstanceDS.COLL_METHOD,
                instanceDto.getCollMethod() != null ? CommonWebUtils.getElementName(instanceDto.getCollMethod().getIdentifier(), instanceDto.getCollMethod().getDescription()) : "");
        ((ExternalItemListItem) productionDescriptorsForm.getItem(InstanceDS.INFORMATION_SUPPLIERS)).setExternalItems(instanceDto.getInformationSuppliers());
        ((ExternalItemListItem) productionDescriptorsForm.getItem(InstanceDS.FREQ_COLL)).setExternalItems(instanceDto.getFreqColl());
        productionDescriptorsForm.setValue(InstanceDS.DATA_VALIDATION, RecordUtils.getInternationalStringRecord(instanceDto.getDataValidation()));
        productionDescriptorsForm.setValue(InstanceDS.DATA_COMPILATION, RecordUtils.getInternationalStringRecord(instanceDto.getDataCompilation()));
        productionDescriptorsForm.setValue(InstanceDS.ADJUSTMENT, RecordUtils.getInternationalStringRecord(instanceDto.getAdjustment()));
        productionDescriptorsForm.setValue(InstanceDS.COST_BURDEN, RecordUtils.getInternationalStringRecord(instanceDto.getCostBurden()));

        productionDescriptorsForm.setValue(InstanceDS.COST, OperationsListUtils.getCostDtoListToString(instanceDto.getCost()));

        // DIFFUSION AND PUBLICATION

        diffusionViewForm.setValue(InstanceDS.INVENTORY_DATE, instanceDto.getInventoryDate());

        // QUALITY DESCRIPTORS

        qualityViewForm.setValue(InstanceDS.QUALITY_DOC, RecordUtils.getInternationalStringRecord(instanceDto.getQualityDoc()));
        qualityViewForm.setValue(InstanceDS.QUALITY_ASSURE, RecordUtils.getInternationalStringRecord(instanceDto.getQualityAssure()));
        qualityViewForm.setValue(InstanceDS.QUALITY_ASSMNT, RecordUtils.getInternationalStringRecord(instanceDto.getQualityAssmnt()));
        qualityViewForm.setValue(InstanceDS.USER_NEEDS, RecordUtils.getInternationalStringRecord(instanceDto.getUserNeeds()));
        qualityViewForm.setValue(InstanceDS.USER_SAT, RecordUtils.getInternationalStringRecord(instanceDto.getUserSat()));
        qualityViewForm.setValue(InstanceDS.COMPLETENESS, RecordUtils.getInternationalStringRecord(instanceDto.getCompleteness()));
        qualityViewForm.setValue(InstanceDS.TIMELINESS, RecordUtils.getInternationalStringRecord(instanceDto.getTimeliness()));
        qualityViewForm.setValue(InstanceDS.PUNCTUALITY, RecordUtils.getInternationalStringRecord(instanceDto.getPunctuality()));
        qualityViewForm.setValue(InstanceDS.ACCURACY_OVERALL, RecordUtils.getInternationalStringRecord(instanceDto.getAccuracyOverall()));
        qualityViewForm.setValue(InstanceDS.SAMPLING_ERROR, RecordUtils.getInternationalStringRecord(instanceDto.getSamplingErr()));
        qualityViewForm.setValue(InstanceDS.NONSAMPLING_ERR, RecordUtils.getInternationalStringRecord(instanceDto.getNonsamplingErr()));
        qualityViewForm.setValue(InstanceDS.COHER_X_DOM, RecordUtils.getInternationalStringRecord(instanceDto.getCoherXDomain()));
        qualityViewForm.setValue(InstanceDS.COHER_INTERNAL, RecordUtils.getInternationalStringRecord(instanceDto.getCoherInternal()));
        qualityViewForm.redraw();
        qualityViewForm.setRedrawOnResize(true);

        // ANNOTATIONS

        annotationsViewForm.setValue(InstanceDS.COMMENTS, RecordUtils.getInternationalStringRecord(instanceDto.getComment()));
        annotationsViewForm.setValue(InstanceDS.NOTES, RecordUtils.getInternationalStringRecord(instanceDto.getNotes()));
    }

    private void setInstanceEditionMode(InstanceDto instanceDto) {

        String[] requiredFieldsToNextProcStatus = RequiredFieldUtils.getInstanceRequiredFieldsToNextProcStatus(instanceDto.getProcStatus());

        // IDENTIFIERS

        code.setValue(instanceDto.getCode());
        identifiersEditionForm.setValue(InstanceDS.CODE_VIEW, instanceDto.getCode());
        title.setValue(instanceDto.getTitle());
        acronym.setValue(instanceDto.getAcronym());
        identifiersEditionForm.setValue(InstanceDS.URN, instanceDto.getUrn());
        identifiersEditionForm.setRequiredTitleSuffix(requiredFieldsToNextProcStatus);
        identifiersEditionForm.markForRedraw();

        // CONTENT CLASSIFIERS

        // CONTENT DESCRIPTORS

        dataDescriptionItem.setValue(instanceDto.getDataDescription());
        statisticalPopulationItem.setValue(instanceDto.getStatisticalPopulation());

        ((ExternalItemListItem) contentDescriptorsEditionForm.getItem(InstanceDS.STATISTICAL_UNIT)).setExternalItems(instanceDto.getStatisticalUnit());

        ((ExternalItemListItem) contentDescriptorsEditionForm.getItem(InstanceDS.GEOGRAPHIC_GRANULARITIES)).setExternalItems(instanceDto.getGeographicGranularity());

        geographicalComparabilityItem.setValue(instanceDto.getGeographicComparability());

        ((ExternalItemListItem) contentDescriptorsEditionForm.getItem(InstanceDS.TEMPORAL_GRANULARITIES)).setExternalItems(instanceDto.getTemporalGranularity());

        temporalComparabilityItem.setValue(instanceDto.getTemporalComparability());
        contentDescriptorsEditionForm.setValue(InstanceDS.BASE_PERIOD, instanceDto.getBasePeriod());

        ((ExternalItemListItem) contentDescriptorsEditionForm.getItem(InstanceDS.MEASURES)).setExternalItems(instanceDto.getUnitMeasure());

        contentDescriptorsEditionForm.setValue(InstanceDS.STAT_CONC_DEF_DESCRIPTION, RecordUtils.getInternationalStringRecord(instanceDto.getStatConcDef()));

        ((ExternalItemListItem) contentDescriptorsEditionForm.getItem(InstanceDS.STAT_CONC_DEF)).setExternalItems(instanceDto.getStatConcDefList());

        contentDescriptorsEditionForm.setValue(InstanceDS.CLASS_SYSTEM_DESCRIPTION, RecordUtils.getInternationalStringRecord(instanceDto.getClassSystem()));
        ((ExternalItemListItem) contentDescriptorsEditionForm.getItem(InstanceDS.CLASS_SYSTEM_LIST)).setExternalItems(instanceDto.getClassSystemList());

        contentDescriptorsEditionForm.setRequiredTitleSuffix(requiredFieldsToNextProcStatus);
        contentDescriptorsEditionForm.markForRedraw();

        // CLASS DESCRIPTORS

        instanceTypeItem.setValue(instanceDto.getInstanceType() != null ? instanceDto.getInstanceType().getId().toString() : "");
        classDescriptorsEditionForm.setRequiredTitleSuffix(requiredFieldsToNextProcStatus);
        classDescriptorsEditionForm.markForRedraw();

        // PRODUCTION DESCRIPTORS

        productionDescriptorsEditionForm.setValue(InstanceDS.CREATED_DATE, instanceDto.getCreatedDate());
        productionDescriptorsEditionForm.setValue(InstanceDS.INTERNAL_INVENTORY_DATE, instanceDto.getInventoryDate());

        productionDescriptorsEditionForm.setValue(InstanceDS.PROC_STATUS, CommonUtils.getProcStatusName(instanceDto.getProcStatus()));
        productionDescriptorsEditionForm.setValue(InstanceDS.PROC_STATUS_VIEW, instanceDto.getProcStatus().getName());

        productionDescriptorsEditionForm.setValue(InstanceDS.DOC_METHOD, RecordUtils.getInternationalStringRecord(instanceDto.getDocMethod()));
        surveySourceItem.setValue(instanceDto.getSurveySource() != null ? instanceDto.getSurveySource().getId() : "");
        collMethodItem.setValue(instanceDto.getCollMethod() != null ? instanceDto.getCollMethod().getId() : "");
        ((ExternalItemListItem) productionDescriptorsEditionForm.getItem(InstanceDS.INFORMATION_SUPPLIERS)).setExternalItems(instanceDto.getInformationSuppliers());
        ((ExternalItemListItem) productionDescriptorsEditionForm.getItem(InstanceDS.FREQ_COLL)).setExternalItems(instanceDto.getFreqColl());
        productionDescriptorsEditionForm.setValue(InstanceDS.DATA_VALIDATION, RecordUtils.getInternationalStringRecord(instanceDto.getDataValidation()));
        productionDescriptorsEditionForm.setValue(InstanceDS.DATA_COMPILATION, RecordUtils.getInternationalStringRecord(instanceDto.getDataCompilation()));
        productionDescriptorsEditionForm.setValue(InstanceDS.ADJUSTMENT, RecordUtils.getInternationalStringRecord(instanceDto.getAdjustment()));
        productionDescriptorsEditionForm.setValue(InstanceDS.COST_BURDEN, RecordUtils.getInternationalStringRecord(instanceDto.getCostBurden()));
        costItem.setValues(getCostIds(instanceDto.getCost()));
        productionDescriptorsEditionForm.setRequiredTitleSuffix(requiredFieldsToNextProcStatus);
        productionDescriptorsEditionForm.markForRedraw();

        // DIFFUSION AND PUBLICATION

        diffusionEditionForm.setValue(InstanceDS.INVENTORY_DATE, instanceDto.getInventoryDate());
        diffusionEditionForm.setRequiredTitleSuffix(requiredFieldsToNextProcStatus);
        diffusionEditionForm.markForRedraw();

        // QUALITY DESCRIPTORS

        qualityEditionForm.setValue(InstanceDS.QUALITY_DOC, RecordUtils.getInternationalStringRecord(instanceDto.getQualityDoc()));
        qualityEditionForm.setValue(InstanceDS.QUALITY_ASSURE, RecordUtils.getInternationalStringRecord(instanceDto.getQualityAssure()));
        qualityEditionForm.setValue(InstanceDS.QUALITY_ASSMNT, RecordUtils.getInternationalStringRecord(instanceDto.getQualityAssmnt()));
        qualityEditionForm.setValue(InstanceDS.USER_NEEDS, RecordUtils.getInternationalStringRecord(instanceDto.getUserNeeds()));
        qualityEditionForm.setValue(InstanceDS.USER_SAT, RecordUtils.getInternationalStringRecord(instanceDto.getUserSat()));
        qualityEditionForm.setValue(InstanceDS.COMPLETENESS, RecordUtils.getInternationalStringRecord(instanceDto.getCompleteness()));
        qualityEditionForm.setValue(InstanceDS.TIMELINESS, RecordUtils.getInternationalStringRecord(instanceDto.getTimeliness()));
        qualityEditionForm.setValue(InstanceDS.PUNCTUALITY, RecordUtils.getInternationalStringRecord(instanceDto.getPunctuality()));
        qualityEditionForm.setValue(InstanceDS.ACCURACY_OVERALL, RecordUtils.getInternationalStringRecord(instanceDto.getAccuracyOverall()));
        qualityEditionForm.setValue(InstanceDS.SAMPLING_ERROR, RecordUtils.getInternationalStringRecord(instanceDto.getSamplingErr()));
        qualityEditionForm.setValue(InstanceDS.NONSAMPLING_ERR, RecordUtils.getInternationalStringRecord(instanceDto.getNonsamplingErr()));
        qualityEditionForm.setValue(InstanceDS.COHER_X_DOM, RecordUtils.getInternationalStringRecord(instanceDto.getCoherXDomain()));
        qualityEditionForm.setValue(InstanceDS.COHER_INTERNAL, RecordUtils.getInternationalStringRecord(instanceDto.getCoherInternal()));
        qualityEditionForm.setRequiredTitleSuffix(requiredFieldsToNextProcStatus);
        qualityEditionForm.markForRedraw();

        // ANNOTATIONS

        annotationsEditionForm.setValue(InstanceDS.COMMENTS, RecordUtils.getInternationalStringRecord(instanceDto.getComment()));
        annotationsEditionForm.setValue(InstanceDS.NOTES, RecordUtils.getInternationalStringRecord(instanceDto.getNotes()));
        annotationsEditionForm.setRequiredTitleSuffix(requiredFieldsToNextProcStatus);
        annotationsEditionForm.markForRedraw();

        identifiersEditionForm.markForRedraw();
        productionDescriptorsEditionForm.markForRedraw();
    }

    @Override
    public HasClickHandlers getPublishInstanceInternally() {
        return mainFormLayout.getPublishInternally();
    }

    @Override
    public HasClickHandlers getPublishInstanceExternally() {
        return mainFormLayout.getPublishExternally();
    }

    private String[] getCostIds(Set<CostDto> costDtos) {
        List<String> list = new ArrayList<String>();
        for (CostDto costDto : costDtos) {
            list.add(costDto.getId().toString());
        }
        return list.toArray(new String[0]);
    }

    @Override
    public void setOperationsLists(List<InstanceTypeDto> instanceTypeDtos, List<SurveySourceDto> surveySourceDtos, List<CollMethodDto> collMethodDtos, List<CostDto> costDtos) {
        this.instanceTypeDtos = instanceTypeDtos;
        this.surveySourceDtos = surveySourceDtos;
        this.collMethodDtos = collMethodDtos;
        this.costDtos = costDtos;
        instanceTypeItem.setValueMap(OperationsListUtils.getInstanceTypeHashMap(instanceTypeDtos));
        surveySourceItem.setValueMap(OperationsListUtils.getSurveySourceHashMap(surveySourceDtos));
        collMethodItem.setValueMap(OperationsListUtils.getCollMethodsHashMap(collMethodDtos));
        costItem.setValueMap(OperationsListUtils.getCostHashMap(costDtos));
    }

    private void setTranslationsShowed(boolean translationsShowed) {
        // Set translationsShowed value to international fields
        identifiersViewForm.setTranslationsShowed(translationsShowed);
        identifiersEditionForm.setTranslationsShowed(translationsShowed);
        contentDescriptorsForm.setTranslationsShowed(translationsShowed);
        contentDescriptorsEditionForm.setTranslationsShowed(translationsShowed);
        productionDescriptorsForm.setTranslationsShowed(translationsShowed);
        productionDescriptorsEditionForm.setTranslationsShowed(translationsShowed);
        qualityViewForm.setTranslationsShowed(translationsShowed);
        qualityEditionForm.setTranslationsShowed(translationsShowed);
        annotationsViewForm.setTranslationsShowed(translationsShowed);
        annotationsEditionForm.setTranslationsShowed(translationsShowed);
    }

    private boolean canInstanceCodeBeEdited() {
        // Operation code can be edited only when ProcStatus is DRAFT
        return (productionDescriptorsEditionForm.getValue(InstanceDS.PROC_STATUS_VIEW) != null && ProcStatusEnum.DRAFT.toString().equals(
                productionDescriptorsEditionForm.getValue(InstanceDS.PROC_STATUS_VIEW)));
    }

    public boolean isInstanceInternallyPublished() {
        return ProcStatusEnum.PUBLISH_INTERNALLY.equals(instanceDto.getProcStatus());
    }

    public boolean isInstanceExternallyPublished() {
        return ProcStatusEnum.PUBLISH_EXTERNALLY.equals(instanceDto.getProcStatus());
    }

    // ------------------------------------------------------------------------------------------------------------
    // EXTERNAL RESOURCES DATA SETTERS
    // ------------------------------------------------------------------------------------------------------------

    @Override
    public void setItemSchemes(String formItemName, ExternalItemsResult result) {
        if (StringUtils.equals(InstanceDS.INFORMATION_SUPPLIERS, formItemName)) {
            ((SearchMultipleSrmItemsItem) productionDescriptorsEditionForm.getItem(formItemName)).setItemSchemes(result);

        } else if (StringUtils.equals(InstanceDS.STATISTICAL_UNIT, formItemName)) {
            ((SearchMultipleSrmItemsItem) contentDescriptorsEditionForm.getItem(formItemName)).setItemSchemes(result);

        } else if (StringUtils.equals(InstanceDS.CLASS_SYSTEM_LIST, formItemName)) {
            ((SearchMultipleSrmItemSchemesItem) contentDescriptorsEditionForm.getItem(formItemName)).setSourceExternalItems(result);

        } else if (StringUtils.equals(InstanceDS.GEOGRAPHIC_GRANULARITIES, formItemName)) {
            ((SearchMultipleSrmItemsItem) contentDescriptorsEditionForm.getItem(formItemName)).setItemSchemes(result);

        } else if (StringUtils.equals(InstanceDS.TEMPORAL_GRANULARITIES, formItemName)) {
            ((SearchMultipleSrmItemsItem) contentDescriptorsEditionForm.getItem(formItemName)).setItemSchemes(result);

        } else if (StringUtils.equals(InstanceDS.FREQ_COLL, formItemName)) {
            ((SearchMultipleSrmItemsItem) productionDescriptorsEditionForm.getItem(formItemName)).setItemSchemes(result);

        } else if (StringUtils.equals(InstanceDS.STAT_CONC_DEF, formItemName)) {
            ((SearchMultipleSrmItemsItem) contentDescriptorsEditionForm.getItem(formItemName)).setItemSchemes(result);

        } else if (StringUtils.equals(InstanceDS.MEASURES, formItemName)) {
            ((SearchMultipleSrmItemsItem) contentDescriptorsEditionForm.getItem(formItemName)).setItemSchemes(result);
        }
    }

    @Override
    public void setItems(String formItemName, ExternalItemsResult result) {
        if (StringUtils.equals(InstanceDS.INFORMATION_SUPPLIERS, formItemName)) {
            ((SearchMultipleSrmItemsItem) productionDescriptorsEditionForm.getItem(formItemName)).setItems(result);

        } else if (StringUtils.equals(InstanceDS.STATISTICAL_UNIT, formItemName)) {
            ((SearchMultipleSrmItemsItem) contentDescriptorsEditionForm.getItem(formItemName)).setItems(result);

        } else if (StringUtils.equals(InstanceDS.GEOGRAPHIC_GRANULARITIES, formItemName)) {
            ((SearchMultipleSrmItemsItem) contentDescriptorsEditionForm.getItem(formItemName)).setItems(result);

        } else if (StringUtils.equals(InstanceDS.TEMPORAL_GRANULARITIES, formItemName)) {
            ((SearchMultipleSrmItemsItem) contentDescriptorsEditionForm.getItem(formItemName)).setItems(result);

        } else if (StringUtils.equals(InstanceDS.FREQ_COLL, formItemName)) {
            ((SearchMultipleSrmItemsItem) productionDescriptorsEditionForm.getItem(formItemName)).setItems(result);

        } else if (StringUtils.equals(InstanceDS.STAT_CONC_DEF, formItemName)) {
            ((SearchMultipleSrmItemsItem) contentDescriptorsEditionForm.getItem(formItemName)).setItems(result);

        } else if (StringUtils.equals(InstanceDS.MEASURES, formItemName)) {
            ((SearchMultipleSrmItemsItem) contentDescriptorsEditionForm.getItem(formItemName)).setItems(result);
        }
    }

    // ------------------------------------------------------------------------------------------------------------
    // EXTERNAL RESOURCES ITEMS
    // ------------------------------------------------------------------------------------------------------------

    private SearchMultipleSrmItemsItem createStatisticalUnitsItem(final String name, String title) {
        final SearchMultipleSrmItemsItem item = new SearchMultipleConceptsForStatisticalUnitItem(name, title, new MultipleExternalResourceAction() {

            @Override
            public List<ExternalItemDto> getExternalItemsPreviouslySelected() {
                return new ArrayList<ExternalItemDto>(((SearchMultipleSrmItemsItem) contentDescriptorsEditionForm.getItem(name)).getExternalItemDtos());
            }
        });
        com.smartgwt.client.widgets.form.fields.events.ClickHandler clickHandler = new com.smartgwt.client.widgets.form.fields.events.ClickHandler() {

            @Override
            public void onClick(com.smartgwt.client.widgets.form.fields.events.ClickEvent event) {
                List<ExternalItemDto> concepts = item.getSelectedItems();
                item.markSearchWindowForDestroy();
                ((SearchMultipleSrmItemsItem) contentDescriptorsEditionForm.getItem(name)).setExternalItems(concepts);
                contentDescriptorsEditionForm.markForRedraw();
            }
        };
        item.setSaveClickHandler(clickHandler);
        return item;
    }

    private SearchMultipleSrmItemsItem createInformationSuppliersItem(final String name, String title) {
        final SearchMultipleSrmItemsItem item = new SearchMultipleDataProvidersItem(name, title, new MultipleExternalResourceAction() {

            @Override
            public List<ExternalItemDto> getExternalItemsPreviouslySelected() {
                return new ArrayList<ExternalItemDto>(((SearchMultipleSrmItemsItem) productionDescriptorsEditionForm.getItem(name)).getExternalItemDtos());
            }
        });
        com.smartgwt.client.widgets.form.fields.events.ClickHandler clickHandler = new com.smartgwt.client.widgets.form.fields.events.ClickHandler() {

            @Override
            public void onClick(com.smartgwt.client.widgets.form.fields.events.ClickEvent event) {
                List<ExternalItemDto> dataProviders = item.getSelectedItems();
                item.markSearchWindowForDestroy();
                ((SearchMultipleSrmItemsItem) productionDescriptorsEditionForm.getItem(name)).setExternalItems(dataProviders);
                productionDescriptorsEditionForm.markForRedraw();
            }
        };
        item.setSaveClickHandler(clickHandler);
        return item;
    }

    private ExternalItemListItem createClassSystemItem(final String name, String title) {
        final SearchMultipleSrmItemSchemesItem item = new SearchMultipleSrmItemSchemesItem(name, title, new MultipleExternalResourceAction() {

            @Override
            public List<ExternalItemDto> getExternalItemsPreviouslySelected() {
                return ((ExternalItemListItem) contentDescriptorsEditionForm.getItem(name)).getExternalItemDtos();
            }
        });
        com.smartgwt.client.widgets.form.fields.events.ClickHandler clickHandler = new com.smartgwt.client.widgets.form.fields.events.ClickHandler() {

            @Override
            public void onClick(com.smartgwt.client.widgets.form.fields.events.ClickEvent event) {
                List<ExternalItemDto> codelists = item.getSelectedItemSchemes();
                item.markSearchWindowForDestroy();
                ((SearchMultipleSrmItemSchemesItem) contentDescriptorsEditionForm.getItem(name)).setExternalItems(codelists);
                contentDescriptorsEditionForm.markForRedraw();
            }
        };
        item.setSaveClickHandler(clickHandler);
        return item;
    }

    private ExternalItemListItem createGeographicGranularities(final String name, String title) {
        final SearchMultipleSrmItemsItem item = new SearchMultipleCodesItem(name, title, new MultipleExternalResourceAction() {

            @Override
            public List<ExternalItemDto> getExternalItemsPreviouslySelected() {
                return ((ExternalItemListItem) contentDescriptorsEditionForm.getItem(name)).getExternalItemDtos();
            }
        });
        com.smartgwt.client.widgets.form.fields.events.ClickHandler clickHandler = new com.smartgwt.client.widgets.form.fields.events.ClickHandler() {

            @Override
            public void onClick(com.smartgwt.client.widgets.form.fields.events.ClickEvent event) {
                List<ExternalItemDto> codes = item.getSelectedItems();
                item.markSearchWindowForDestroy();
                ((SearchMultipleSrmItemsItem) contentDescriptorsEditionForm.getItem(name)).setExternalItems(codes);
                contentDescriptorsEditionForm.markForRedraw();
            }
        };
        item.setSaveClickHandler(clickHandler);

        String defaultCodelistUrn = ConfigurationPropertiesUtils.getInstanceDefaultCodelistForGeographicGranularity();

        // If the default codelist has not been specified (in the data directory), do not show the window to select the codes
        if (StringUtils.isBlank(defaultCodelistUrn)) {
            item.getSearchIconClickHandlerRegistration().removeHandler();
            item.getSearchIcon().addFormItemClickHandler(new FormItemClickHandler() {

                @Override
                public void onFormItemClick(FormItemIconClickEvent event) {
                    InformationWindow infoWindow = new InformationWindow(getConstants().instanceGranularityDefaultCodelistTitle(), getConstants().instanceGranularityDefaultCodelistInfoMessage());
                    infoWindow.show();
                }
            });
        }

        item.setDefaultItemSchemeUrn(defaultCodelistUrn);

        // Disable the filter by codelist. The codes for the granularity must belongs to the codelist specified in the data directory.
        item.hideSearchWindowFilters();

        // Show an information label to inform that the codes show belongs to the codelist specified in the data directory.
        item.showSearchWindowInformationLabel(getMessages().instanceGeographicGranularityCodesInfoMesage());

        return item;
    }

    private ExternalItemListItem createTemporalGranularities(final String name, String title) {
        final SearchMultipleSrmItemsItem item = new SearchMultipleCodesItem(name, title, new MultipleExternalResourceAction() {

            @Override
            public List<ExternalItemDto> getExternalItemsPreviouslySelected() {
                return ((ExternalItemListItem) contentDescriptorsEditionForm.getItem(name)).getExternalItemDtos();
            }
        });
        com.smartgwt.client.widgets.form.fields.events.ClickHandler clickHandler = new com.smartgwt.client.widgets.form.fields.events.ClickHandler() {

            @Override
            public void onClick(com.smartgwt.client.widgets.form.fields.events.ClickEvent event) {
                List<ExternalItemDto> codes = item.getSelectedItems();
                item.markSearchWindowForDestroy();
                ((SearchMultipleSrmItemsItem) contentDescriptorsEditionForm.getItem(name)).setExternalItems(codes);
                contentDescriptorsEditionForm.markForRedraw();
            }
        };
        item.setSaveClickHandler(clickHandler);

        String defaultCodelistUrn = ConfigurationPropertiesUtils.getInstanceDefaultCodelistForTemporalGranularity();

        // If the default codelist has not been specified (in the data directory), do not show the window to select the codes
        if (StringUtils.isBlank(defaultCodelistUrn)) {
            item.getSearchIconClickHandlerRegistration().removeHandler();
            item.getSearchIcon().addFormItemClickHandler(new FormItemClickHandler() {

                @Override
                public void onFormItemClick(FormItemIconClickEvent event) {
                    InformationWindow infoWindow = new InformationWindow(getConstants().instanceGranularityDefaultCodelistTitle(), getConstants().instanceGranularityDefaultCodelistInfoMessage());
                    infoWindow.show();
                }
            });
        }

        // Set default codelist
        item.setDefaultItemSchemeUrn(defaultCodelistUrn);

        // Disable the filter by codelist. The codes for the granularity must belongs to the codelist specified in the data directory.
        item.hideSearchWindowFilters();

        // Show an information label to inform that the codes show belongs to the codelist specified in the data directory.
        item.showSearchWindowInformationLabel(getMessages().instanceTemporalGranularityCodesInfoMesage());

        return item;
    }

    private ExternalItemListItem createFreqColl(final String name, String title) {
        final SearchMultipleSrmItemsItem item = new SearchMultipleCodesItem(name, title, new MultipleExternalResourceAction() {

            @Override
            public List<ExternalItemDto> getExternalItemsPreviouslySelected() {
                return ((ExternalItemListItem) productionDescriptorsEditionForm.getItem(name)).getExternalItemDtos();
            }
        });
        com.smartgwt.client.widgets.form.fields.events.ClickHandler clickHandler = new com.smartgwt.client.widgets.form.fields.events.ClickHandler() {

            @Override
            public void onClick(com.smartgwt.client.widgets.form.fields.events.ClickEvent event) {
                List<ExternalItemDto> codes = item.getSelectedItems();
                item.markSearchWindowForDestroy();
                ((SearchMultipleSrmItemsItem) productionDescriptorsEditionForm.getItem(name)).setExternalItems(codes);
                productionDescriptorsEditionForm.markForRedraw();
            }
        };
        item.setSaveClickHandler(clickHandler);
        item.setDefaultItemSchemeUrn(ConfigurationPropertiesUtils.getInstanceDefaultCodelistForFreqColl());
        return item;
    }

    private ExternalItemListItem createStatConcDef(final String name, String title) {
        final SearchMultipleConceptsAndConceptSchemesForStatConcDefItem item = new SearchMultipleConceptsAndConceptSchemesForStatConcDefItem(name, title, new MultipleExternalResourceAction() {

            @Override
            public List<ExternalItemDto> getExternalItemsPreviouslySelected() {
                return ((ExternalItemListItem) contentDescriptorsEditionForm.getItem(name)).getExternalItemDtos();
            }
        });
        com.smartgwt.client.widgets.form.fields.events.ClickHandler clickHandler = new com.smartgwt.client.widgets.form.fields.events.ClickHandler() {

            @Override
            public void onClick(com.smartgwt.client.widgets.form.fields.events.ClickEvent event) {
                List<ExternalItemDto> resources = item.getSelectedItems();
                item.markSearchWindowForDestroy();
                ((SearchMultipleSrmItemsItem) contentDescriptorsEditionForm.getItem(name)).setExternalItems(resources);
                contentDescriptorsEditionForm.markForRedraw();
            }
        };
        item.setSaveClickHandler(clickHandler);
        return item;
    }

    private ExternalItemListItem createMeasures(final String name, String title) {
        final SearchMultipleConceptsAndConceptSchemesForMeasuresItem item = new SearchMultipleConceptsAndConceptSchemesForMeasuresItem(name, title, new MultipleExternalResourceAction() {

            @Override
            public List<ExternalItemDto> getExternalItemsPreviouslySelected() {
                return ((ExternalItemListItem) contentDescriptorsEditionForm.getItem(name)).getExternalItemDtos();
            }
        });
        com.smartgwt.client.widgets.form.fields.events.ClickHandler clickHandler = new com.smartgwt.client.widgets.form.fields.events.ClickHandler() {

            @Override
            public void onClick(com.smartgwt.client.widgets.form.fields.events.ClickEvent event) {
                List<ExternalItemDto> resources = item.getSelectedItems();
                item.markSearchWindowForDestroy();
                ((SearchMultipleSrmItemsItem) contentDescriptorsEditionForm.getItem(name)).setExternalItems(resources);
                contentDescriptorsEditionForm.markForRedraw();
            }
        };
        item.setSaveClickHandler(clickHandler);
        return item;
    }
}
