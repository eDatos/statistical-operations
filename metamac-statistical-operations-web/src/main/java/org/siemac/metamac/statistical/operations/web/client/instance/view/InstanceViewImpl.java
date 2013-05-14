package org.siemac.metamac.statistical.operations.web.client.instance.view;

import static org.siemac.metamac.statistical.operations.web.client.OperationsWeb.getConstants;
import static org.siemac.metamac.statistical.operations.web.client.OperationsWeb.getCoreMessages;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Set;

import org.siemac.metamac.core.common.dto.ExternalItemDto;
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
import org.siemac.metamac.statistical.operations.web.client.utils.OperationsListUtils;
import org.siemac.metamac.statistical.operations.web.client.widgets.InstanceMainFormLayout;
import org.siemac.metamac.web.common.client.utils.CommonWebUtils;
import org.siemac.metamac.web.common.client.utils.ExternalItemUtils;
import org.siemac.metamac.web.common.client.utils.FormItemUtils;
import org.siemac.metamac.web.common.client.utils.InternationalStringUtils;
import org.siemac.metamac.web.common.client.utils.RecordUtils;
import org.siemac.metamac.web.common.client.utils.TimeVariableWebUtils;
import org.siemac.metamac.web.common.client.widgets.form.GroupDynamicForm;
import org.siemac.metamac.web.common.client.widgets.form.fields.CustomSelectItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.ExternalMultipleSelectItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.MultiLanguageTextItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.MultilanguageRichTextEditorItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.RequiredTextItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.ViewMultiLanguageTextItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.ViewTextItem;

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
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
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
    private GroupDynamicForm                contentViewForm;
    private GroupDynamicForm                contentEditionForm;
    private MultiLanguageTextItem           dataDescriptionItem;
    private MultiLanguageTextItem           statisticalPopulationItem;
    private ExternalMultipleSelectItem      statisticalUnitItem;
    private CustomSelectItem                geographicalGranularityItem;
    private MultiLanguageTextItem           geographicalComparabilityItem;
    private CustomSelectItem                temporalGranularityItem;
    private MultiLanguageTextItem           temporalComparabilityItem;
    private CustomSelectItem                unitMeasureItem;
    private MultiLanguageTextItem           statConcDefItem;
    private CustomSelectItem                statConcDefListItem;
    private MultiLanguageTextItem           classSystemItem;
    private CustomSelectItem                classSystemListItem;

    // CLASS DESCRIPTORS
    private GroupDynamicForm                classViewForm;
    private GroupDynamicForm                classEditionForm;
    private CustomSelectItem                instanceTypeItem;

    // PRODUCTION DESCRIPTORS
    private GroupDynamicForm                productionViewForm;
    private GroupDynamicForm                productionEditionForm;
    private MultilanguageRichTextEditorItem docMethodItem;
    private CustomSelectItem                collMethodItem;
    private CustomSelectItem                surveySourceItem;
    private ExternalMultipleSelectItem      infSuppliersOrganItem;
    private ExternalMultipleSelectItem      infSuppliersConceptsItem;
    private CustomSelectItem                freqCollItem;
    private MultilanguageRichTextEditorItem dataValidationItem;
    private MultilanguageRichTextEditorItem dataCompilationItem;
    private MultilanguageRichTextEditorItem adjustmentItem;
    private MultilanguageRichTextEditorItem costBurdenItem;
    private CustomSelectItem                costItem;

    private ViewMultiLanguageTextItem       staticDocMethodItem;
    private ViewMultiLanguageTextItem       staticDataValidationItem;
    private ViewMultiLanguageTextItem       staticDataCompilationItem;
    private ViewMultiLanguageTextItem       staticAdjustmentItem;
    private ViewMultiLanguageTextItem       staticCostBurdenItem;

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

    private ViewMultiLanguageTextItem       staticQualityDocItem;
    private ViewMultiLanguageTextItem       staticQualityAssureItem;
    private ViewMultiLanguageTextItem       staticQualityAssesmentItem;
    private ViewMultiLanguageTextItem       staticUserNeedsItem;
    private ViewMultiLanguageTextItem       staticUserSatItem;
    private ViewMultiLanguageTextItem       staticCompletenessItem;
    private ViewMultiLanguageTextItem       staticTimelinessItem;
    private ViewMultiLanguageTextItem       staticPunctualityItem;
    private ViewMultiLanguageTextItem       staticAccuracyOverallItem;
    private ViewMultiLanguageTextItem       staticSamplingErrItem;
    private ViewMultiLanguageTextItem       statocNonSamplingErrItem;
    private ViewMultiLanguageTextItem       staticCoherXDomItem;
    private ViewMultiLanguageTextItem       staticCoherInternalItem;

    // ANNOTATIONS
    private GroupDynamicForm                annotationsViewForm;
    private GroupDynamicForm                annotationsEditionForm;
    private MultilanguageRichTextEditorItem commentItem;
    private MultilanguageRichTextEditorItem notesItem;

    private ViewMultiLanguageTextItem       staticCommentItem;
    private ViewMultiLanguageTextItem       staticNotesItem;

    private List<ExternalItemDto>           conceptSchemes;
    private List<ExternalItemDto>           codeLists;

    private List<ExternalItemDto>           statisticalUnitConcepts;
    private List<ExternalItemDto>           infSuppliersOrganisations;
    private List<ExternalItemDto>           infSuppliersConcepts;
    private List<ExternalItemDto>           temporalGranularityCodes;
    private List<ExternalItemDto>           freqCollCodes;

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
        // Identifiers
        instanceDto.setCode(code.getValueAsString());
        instanceDto.setTitle(title.getValue());
        instanceDto.setAcronym(acronym.getValue());

        // Content Classifiers

        // Content Descriptors
        instanceDto.setDataDescription(dataDescriptionItem.getValue());
        instanceDto.setStatisticalPopulation(statisticalPopulationItem.getValue());
        instanceDto.getStatisticalUnit().clear();
        instanceDto.getStatisticalUnit().addAll(statisticalUnitItem.getSelectedExternalItems(statisticalUnitConcepts));
        // FIXME instanceDto.setGeographicGranularity(ExternalItemUtils.getExternalItemDtoFromUrn(codeLists, geographicalGranularityItem.getValueAsString()));
        instanceDto.setGeographicComparability(geographicalComparabilityItem.getValue());
        // FIXME instanceDto.setTemporalGranularity(ExternalItemUtils.getExternalItemDtoFromUrn(temporalGranularityCodes, temporalGranularityItem.getValueAsString()));
        instanceDto.setTemporalComparability(temporalComparabilityItem.getValue());
        instanceDto.setBasePeriod(contentEditionForm.getValueAsString(InstanceDS.BASE_PERIOD));
        instanceDto.getUnitMeasure().clear();
        instanceDto.getUnitMeasure().addAll(ExternalItemUtils.getExternalItemDtoListFromUrns(codeLists, unitMeasureItem.getValues()));
        instanceDto.setStatConcDef(statConcDefItem.getValue());
        instanceDto.getStatConcDefList().clear();
        instanceDto.getStatConcDefList().addAll(ExternalItemUtils.getExternalItemDtoListFromUrns(conceptSchemes, statConcDefListItem.getValues()));
        instanceDto.setClassSystem(classSystemItem.getValue());
        instanceDto.getClassSystemList().clear();
        instanceDto.getClassSystemList().addAll(ExternalItemUtils.getExternalItemDtoListFromUrns(codeLists, classSystemListItem.getValues()));

        // Class descriptors
        instanceDto.setInstanceType(OperationsListUtils.getInstanceTypeDto(instanceTypeItem.getValueAsString(), instanceTypeDtos));

        // Production descriptors
        instanceDto.setDocMethod(docMethodItem.getValue());
        instanceDto.setSurveySource(OperationsListUtils.getSurveySourceDto(surveySourceItem.getValueAsString(), surveySourceDtos));
        instanceDto.setCollMethod(OperationsListUtils.getCollMethodDto(collMethodItem.getValueAsString(), collMethodDtos));
        instanceDto.getInformationSuppliers().clear();
        instanceDto.getInformationSuppliers().addAll(infSuppliersOrganItem.getSelectedExternalItems(infSuppliersOrganisations));
        instanceDto.getInformationSuppliers().addAll(infSuppliersConceptsItem.getSelectedExternalItems(infSuppliersConcepts));
        instanceDto.getFreqColl().clear();
        instanceDto.getFreqColl().addAll(ExternalItemUtils.getExternalItemDtoListFromUrns(freqCollCodes, freqCollItem.getValues()));
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

        contentEditionForm.validate(false) && classEditionForm.validate(false) && productionEditionForm.validate(false) && diffusionEditionForm.validate(false) && qualityEditionForm.validate(false)
                && annotationsEditionForm.validate(false);
    }

    private void createViewForm() {
        // Identifiers
        identifiersViewForm = new GroupDynamicForm(getConstants().operationIdentifiers());
        ViewTextItem identifier = new ViewTextItem(InstanceDS.CODE, getCoreMessages().instance_code());
        ViewMultiLanguageTextItem title = new ViewMultiLanguageTextItem(InstanceDS.TITLE, getCoreMessages().instance_title());
        ViewMultiLanguageTextItem alternativeTitle = new ViewMultiLanguageTextItem(InstanceDS.ACRONYM, getCoreMessages().instance_acronym());
        identifiersViewForm.setFields(identifier, title, alternativeTitle);

        // Content classifiers

        // Content descriptors
        contentViewForm = new GroupDynamicForm(getConstants().instanceContentDescriptors());
        ViewMultiLanguageTextItem dataDescription = new ViewMultiLanguageTextItem(InstanceDS.DATA_DESCRIPTION, getConstants().instanceDataDescription());
        ViewMultiLanguageTextItem statisticalPopulation = new ViewMultiLanguageTextItem(InstanceDS.STATISTICAL_POPULATION, getCoreMessages().instance_statistical_population());
        ViewTextItem statisticalUnit = new ViewTextItem(InstanceDS.STATISTIAL_UNIT, getCoreMessages().instance_statistical_unit());
        ViewTextItem geographicGranularity = new ViewTextItem(InstanceDS.GEOGRAPHIC_GRANULARITY, getCoreMessages().instance_geographic_granularity());
        ViewMultiLanguageTextItem geographicComparability = new ViewMultiLanguageTextItem(InstanceDS.GEOGRAPHIC_COMPARABILITY, getCoreMessages().instance_geographic_comparability());
        ViewTextItem temporalGranularity = new ViewTextItem(InstanceDS.TEMPORAL_GRANULARITY, getCoreMessages().instance_temporal_granularity());
        ViewMultiLanguageTextItem temporalComparability = new ViewMultiLanguageTextItem(InstanceDS.TEMPORAL_COMPARABILITY, getCoreMessages().instance_temporal_comparability());
        ViewTextItem basePeriodItem = new ViewTextItem(InstanceDS.BASE_PERIOD, getCoreMessages().instance_base_period());
        ViewTextItem unitMeasure = new ViewTextItem(InstanceDS.UNIT_MEASURE, getCoreMessages().instance_unit_measure());
        ViewMultiLanguageTextItem statConcDef = new ViewMultiLanguageTextItem(InstanceDS.STAT_CONC_DEF, getCoreMessages().instance_stat_conc_def());
        ViewTextItem statConcDefList = new ViewTextItem(InstanceDS.STAT_CONC_DEF_LIST, getCoreMessages().instance_stat_conc_def_list());
        ViewMultiLanguageTextItem classSystem = new ViewMultiLanguageTextItem(InstanceDS.CLASS_SYSTEM, getCoreMessages().instance_class_system());
        ViewTextItem classSystemList = new ViewTextItem(InstanceDS.CLASS_SYSTEM_LIST, getCoreMessages().instance_class_system_list());
        contentViewForm.setFields(dataDescription, statisticalPopulation, statisticalUnit, geographicGranularity, geographicComparability, temporalGranularity, temporalComparability, basePeriodItem,
                unitMeasure, statConcDef, statConcDefList, classSystem, classSystemList);

        // Class descriptors
        classViewForm = new GroupDynamicForm(getConstants().instanceClassDescriptors());
        ViewTextItem instanceType = new ViewTextItem(InstanceDS.INSTANCE_TYPE, getConstants().instanceType());
        classViewForm.setFields(instanceType);

        // Production descriptors
        productionViewForm = new GroupDynamicForm(getConstants().instanceProductionDescriptors());
        ViewTextItem internalInventoryDate = new ViewTextItem(InstanceDS.INTERNAL_INVENTORY_DATE, getCoreMessages().instance_internal_inventory_date());
        ViewTextItem procStatus = new ViewTextItem(InstanceDS.PROC_STATUS, getCoreMessages().instance_proc_status());
        staticDocMethodItem = new ViewMultiLanguageTextItem(InstanceDS.DOC_METHOD, getCoreMessages().instance_doc_method());
        ViewTextItem surveySource = new ViewTextItem(InstanceDS.SURVEY_SOURCE, getConstants().instanceSurveySource());
        ViewTextItem collMethod = new ViewTextItem(InstanceDS.COLL_METHOD, getConstants().instanceCollMethod());
        ViewTextItem informationSuppliers = new ViewTextItem(InstanceDS.INFORMATION_SUPPLIERS, getCoreMessages().instance_information_suppliers());
        ViewTextItem freqColl = new ViewTextItem(InstanceDS.FREQ_COLL, getCoreMessages().instance_freq_coll());
        staticDataValidationItem = new ViewMultiLanguageTextItem(InstanceDS.DATA_VALIDATION, getCoreMessages().instance_data_validation());
        staticDataCompilationItem = new ViewMultiLanguageTextItem(InstanceDS.DATA_COMPILATION, getCoreMessages().instance_data_compilation());
        staticAdjustmentItem = new ViewMultiLanguageTextItem(InstanceDS.ADJUSTMENT, getCoreMessages().instance_adjustment());
        staticCostBurdenItem = new ViewMultiLanguageTextItem(InstanceDS.COST_BURDEN, getCoreMessages().instance_cost_burden());
        ViewTextItem cost = new ViewTextItem(InstanceDS.COST, getConstants().instanceCost());
        productionViewForm.setFields(internalInventoryDate, procStatus, staticDocMethodItem, surveySource, collMethod, informationSuppliers, freqColl, staticDataValidationItem,
                staticDataCompilationItem, staticAdjustmentItem, staticCostBurdenItem, cost);

        // Diffusion and Publication
        diffusionViewForm = new GroupDynamicForm(getConstants().instanceDiffusionDescriptors());
        ViewTextItem inventoryDate = new ViewTextItem(InstanceDS.INVENTORY_DATE, getCoreMessages().instance_inventory_date());
        diffusionViewForm.setFields(inventoryDate);

        // Quality descriptors
        qualityViewForm = new GroupDynamicForm(getConstants().instanceQualityDescriptors());
        staticQualityDocItem = new ViewMultiLanguageTextItem(InstanceDS.QUALITY_DOC, getCoreMessages().instance_quality_doc());
        staticQualityAssureItem = new ViewMultiLanguageTextItem(InstanceDS.QUALITY_ASSURE, getCoreMessages().instance_quality_assure());
        staticQualityAssesmentItem = new ViewMultiLanguageTextItem(InstanceDS.QUALITY_ASSMNT, getCoreMessages().instance_quality_assmnt());
        staticUserNeedsItem = new ViewMultiLanguageTextItem(InstanceDS.USER_NEEDS, getCoreMessages().instance_user_needs());
        staticUserSatItem = new ViewMultiLanguageTextItem(InstanceDS.USER_SAT, getCoreMessages().instance_user_sat());
        staticCompletenessItem = new ViewMultiLanguageTextItem(InstanceDS.COMPLETENESS, getCoreMessages().instance_completeness());
        staticTimelinessItem = new ViewMultiLanguageTextItem(InstanceDS.TIMELINESS, getCoreMessages().instance_timeliness());
        staticPunctualityItem = new ViewMultiLanguageTextItem(InstanceDS.PUNCTUALITY, getCoreMessages().instance_punctuality());
        staticAccuracyOverallItem = new ViewMultiLanguageTextItem(InstanceDS.ACCURACY_OVERALL, getCoreMessages().instance_accuracy_overall());
        staticSamplingErrItem = new ViewMultiLanguageTextItem(InstanceDS.SAMPLING_ERROR, getCoreMessages().instance_sampling_err());
        statocNonSamplingErrItem = new ViewMultiLanguageTextItem(InstanceDS.NONSAMPLING_ERR, getCoreMessages().instance_nonsampling_err());
        staticCoherXDomItem = new ViewMultiLanguageTextItem(InstanceDS.COHER_X_DOM, getCoreMessages().instance_coher_x_domain());
        staticCoherInternalItem = new ViewMultiLanguageTextItem(InstanceDS.COHER_INTERNAL, getCoreMessages().instance_coher_internal());
        qualityViewForm.setFields(staticQualityDocItem, staticQualityAssureItem, staticQualityAssesmentItem, staticUserNeedsItem, staticUserSatItem, staticCompletenessItem, staticTimelinessItem,
                staticPunctualityItem, staticAccuracyOverallItem, staticSamplingErrItem, statocNonSamplingErrItem, staticCoherXDomItem, staticCoherInternalItem);

        // Annotations
        annotationsViewForm = new GroupDynamicForm(getConstants().instanceAnnotations());
        staticCommentItem = new ViewMultiLanguageTextItem(InstanceDS.COMMENTS, getCoreMessages().instance_comment());
        staticNotesItem = new ViewMultiLanguageTextItem(InstanceDS.NOTES, getCoreMessages().instance_notes());
        annotationsViewForm.setFields(staticCommentItem, staticNotesItem);

        mainFormLayout.addViewCanvas(identifiersViewForm);

        mainFormLayout.addViewCanvas(contentViewForm);
        mainFormLayout.addViewCanvas(classViewForm);
        mainFormLayout.addViewCanvas(productionViewForm);
        mainFormLayout.addViewCanvas(diffusionViewForm);
        mainFormLayout.addViewCanvas(qualityViewForm);
        mainFormLayout.addViewCanvas(annotationsViewForm);
    }

    private void createEditionForm() {
        // Identifiers
        identifiersEditionForm = new GroupDynamicForm(getConstants().instanceIdentifiers());

        code = new RequiredTextItem(InstanceDS.CODE, getCoreMessages().instance_code());
        code.setShowIfCondition(new FormItemIfFunction() {

            @Override
            public boolean execute(FormItem item, Object value, DynamicForm form) {
                return canInstanceCodeBeEdited();
            }
        });
        code.setValidators(CommonWebUtils.getSemanticIdentifierCustomValidator());
        ViewTextItem staticCode = new ViewTextItem(InstanceDS.CODE_VIEW, getCoreMessages().instance_code());
        staticCode.setShowIfCondition(new FormItemIfFunction() {

            @Override
            public boolean execute(FormItem item, Object value, DynamicForm form) {
                return !canInstanceCodeBeEdited();
            }
        });

        title = new MultiLanguageTextItem(InstanceDS.TITLE, getCoreMessages().instance_title());
        title.setRequired(true);
        acronym = new MultiLanguageTextItem(InstanceDS.ACRONYM, getCoreMessages().instance_acronym());
        identifiersEditionForm.setFields(staticCode, code, title, acronym);

        // Content classifiers

        // Content descriptors
        contentEditionForm = new GroupDynamicForm(getConstants().instanceContentDescriptors());
        dataDescriptionItem = new MultiLanguageTextItem(InstanceDS.DATA_DESCRIPTION, getConstants().instanceDataDescription());
        statisticalPopulationItem = new MultiLanguageTextItem(InstanceDS.STATISTICAL_POPULATION, getCoreMessages().instance_statistical_population());
        statisticalUnitItem = new ExternalMultipleSelectItem(InstanceDS.STATISTIAL_UNIT, getCoreMessages().instance_statistical_unit());
        statisticalUnitItem.getSchemeItem().addChangedHandler(new ChangedHandler() {

            @Override
            public void onChanged(ChangedEvent event) {
                if (event.getValue() != null) {
                    getUiHandlers().populateStatisticalUnitConcepts(event.getValue().toString());
                }
            }
        });
        geographicalGranularityItem = new CustomSelectItem(InstanceDS.GEOGRAPHIC_GRANULARITY, getCoreMessages().instance_geographic_granularity());
        geographicalComparabilityItem = new MultiLanguageTextItem(InstanceDS.GEOGRAPHIC_COMPARABILITY, getCoreMessages().instance_geographic_comparability());
        temporalGranularityItem = new CustomSelectItem(InstanceDS.TEMPORAL_GRANULARITY, getCoreMessages().instance_temporal_granularity());
        temporalComparabilityItem = new MultiLanguageTextItem(InstanceDS.TEMPORAL_COMPARABILITY, getCoreMessages().instance_temporal_comparability());
        TextItem basePeriodItem = new TextItem(InstanceDS.BASE_PERIOD, getCoreMessages().instance_base_period());
        basePeriodItem.setValidators(TimeVariableWebUtils.getTimeCustomValidator());
        unitMeasureItem = new CustomSelectItem(InstanceDS.UNIT_MEASURE, getCoreMessages().instance_unit_measure());
        unitMeasureItem.setMultiple(true);
        statConcDefItem = new MultiLanguageTextItem(InstanceDS.STAT_CONC_DEF, getCoreMessages().instance_stat_conc_def());
        statConcDefListItem = new CustomSelectItem(InstanceDS.STAT_CONC_DEF_LIST, getCoreMessages().instance_stat_conc_def_list());
        statConcDefListItem.setMultiple(true);
        classSystemItem = new MultiLanguageTextItem(InstanceDS.CLASS_SYSTEM, getCoreMessages().instance_class_system());
        classSystemListItem = new CustomSelectItem(InstanceDS.CLASS_SYSTEM_LIST, getCoreMessages().instance_class_system_list());
        classSystemListItem.setMultiple(true);
        contentEditionForm.setFields(dataDescriptionItem, statisticalPopulationItem, statisticalUnitItem, geographicalGranularityItem, geographicalComparabilityItem, temporalGranularityItem,
                basePeriodItem, temporalComparabilityItem, unitMeasureItem, statConcDefItem, statConcDefListItem, classSystemItem, classSystemListItem);

        // Class descriptors
        classEditionForm = new GroupDynamicForm(getConstants().instanceClassDescriptors());
        instanceTypeItem = new CustomSelectItem(InstanceDS.INSTANCE_TYPE, getConstants().instanceType());
        // instanceTypeItem.setValidators(getRequiredIfInternallyPublished());
        classEditionForm.setFields(instanceTypeItem);

        // Production descriptors
        productionEditionForm = new GroupDynamicForm(getConstants().instanceProductionDescriptors());
        ViewTextItem internalInventoryDate = new ViewTextItem(InstanceDS.INTERNAL_INVENTORY_DATE, getCoreMessages().instance_internal_inventory_date());

        ViewTextItem procStatus = new ViewTextItem(InstanceDS.PROC_STATUS, getCoreMessages().instance_proc_status());
        ViewTextItem staticProcStatus = new ViewTextItem(InstanceDS.PROC_STATUS_VIEW, getCoreMessages().instance_proc_status());
        staticProcStatus.setShowIfCondition(FormItemUtils.getFalseFormItemIfFunction());

        docMethodItem = new MultilanguageRichTextEditorItem(InstanceDS.DOC_METHOD, getCoreMessages().instance_doc_method());
        surveySourceItem = new CustomSelectItem(InstanceDS.SURVEY_SOURCE, getConstants().instanceSurveySource());
        collMethodItem = new CustomSelectItem(InstanceDS.COLL_METHOD, getConstants().instanceCollMethod());
        infSuppliersOrganItem = new ExternalMultipleSelectItem(InstanceDS.INFORMATION_SUPPLIERS, getConstants().instanceInformationSuppliersOrg());
        infSuppliersOrganItem.getSchemeItem().addChangedHandler(new ChangedHandler() {

            @Override
            public void onChanged(ChangedEvent event) {
                if (event.getValue() != null) {
                    getUiHandlers().populateInfSuppliersOrg(event.getValue().toString());
                }
            }
        });
        infSuppliersConceptsItem = new ExternalMultipleSelectItem(InstanceDS.INFORMATION_SUPPLIERS + "-con", getConstants().instanceInformationSuppliersCon());
        infSuppliersConceptsItem.getSchemeItem().addChangedHandler(new ChangedHandler() {

            @Override
            public void onChanged(ChangedEvent event) {
                getUiHandlers().populateInfSuppliersConcept(event.getValue().toString());
            }
        });
        freqCollItem = new CustomSelectItem(InstanceDS.FREQ_COLL, getCoreMessages().instance_freq_coll());
        freqCollItem.setMultiple(true);
        dataValidationItem = new MultilanguageRichTextEditorItem(InstanceDS.DATA_VALIDATION, getCoreMessages().instance_data_validation());
        dataCompilationItem = new MultilanguageRichTextEditorItem(InstanceDS.DATA_COMPILATION, getCoreMessages().instance_data_compilation());
        adjustmentItem = new MultilanguageRichTextEditorItem(InstanceDS.ADJUSTMENT, getCoreMessages().instance_adjustment());
        costBurdenItem = new MultilanguageRichTextEditorItem(InstanceDS.COST_BURDEN, getCoreMessages().instance_cost_burden());
        costItem = new CustomSelectItem(InstanceDS.COST, getConstants().instanceCost());
        costItem.setMultiple(true);
        productionEditionForm.setFields(internalInventoryDate, staticProcStatus, procStatus, docMethodItem, surveySourceItem, collMethodItem, infSuppliersOrganItem, infSuppliersConceptsItem,
                freqCollItem, dataValidationItem, dataCompilationItem, adjustmentItem, costBurdenItem, costItem);

        // Diffusion and Publication
        diffusionEditionForm = new GroupDynamicForm(getConstants().instanceDiffusionDescriptors());
        ViewTextItem inventoryDate = new ViewTextItem(InstanceDS.INVENTORY_DATE, getCoreMessages().instance_inventory_date());
        diffusionEditionForm.setFields(inventoryDate);

        // Quality Descriptors
        qualityEditionForm = new GroupDynamicForm(getConstants().instanceQualityDescriptors());
        qualityDocItem = new MultilanguageRichTextEditorItem(InstanceDS.QUALITY_DOC, getCoreMessages().instance_quality_doc());
        qualityAssureItem = new MultilanguageRichTextEditorItem(InstanceDS.QUALITY_ASSURE, getCoreMessages().instance_quality_assure());
        qualityAssesmentItem = new MultilanguageRichTextEditorItem(InstanceDS.QUALITY_ASSMNT, getCoreMessages().instance_quality_assmnt());
        userNeedsItem = new MultilanguageRichTextEditorItem(InstanceDS.USER_NEEDS, getCoreMessages().instance_user_needs());
        userSatItem = new MultilanguageRichTextEditorItem(InstanceDS.USER_SAT, getCoreMessages().instance_user_sat());
        completenessItem = new MultilanguageRichTextEditorItem(InstanceDS.COMPLETENESS, getCoreMessages().instance_completeness());
        timelinessItem = new MultilanguageRichTextEditorItem(InstanceDS.TIMELINESS, getCoreMessages().instance_timeliness());
        punctualityItem = new MultilanguageRichTextEditorItem(InstanceDS.PUNCTUALITY, getCoreMessages().instance_punctuality());
        accuracyOverallItem = new MultilanguageRichTextEditorItem(InstanceDS.ACCURACY_OVERALL, getCoreMessages().instance_accuracy_overall());
        samplingErrItem = new MultilanguageRichTextEditorItem(InstanceDS.SAMPLING_ERROR, getCoreMessages().instance_sampling_err());
        nonSamplingErrItem = new MultilanguageRichTextEditorItem(InstanceDS.NONSAMPLING_ERR, getCoreMessages().instance_nonsampling_err());
        coherXDomItem = new MultilanguageRichTextEditorItem(InstanceDS.COHER_X_DOM, getCoreMessages().instance_coher_x_domain());
        coherInternalItem = new MultilanguageRichTextEditorItem(InstanceDS.COHER_INTERNAL, getCoreMessages().instance_coher_internal());
        qualityEditionForm.setFields(qualityDocItem, qualityAssureItem, qualityAssesmentItem, userNeedsItem, userSatItem, completenessItem, timelinessItem, punctualityItem, accuracyOverallItem,
                samplingErrItem, nonSamplingErrItem, coherXDomItem, coherInternalItem);

        // Annotations
        annotationsEditionForm = new GroupDynamicForm(getConstants().instanceAnnotations());
        commentItem = new MultilanguageRichTextEditorItem(InstanceDS.COMMENTS, getCoreMessages().instance_comment());
        notesItem = new MultilanguageRichTextEditorItem(InstanceDS.NOTES, getCoreMessages().instance_notes());
        annotationsEditionForm.setFields(commentItem, notesItem);

        mainFormLayout.addEditionCanvas(identifiersEditionForm);

        mainFormLayout.addEditionCanvas(contentEditionForm);
        mainFormLayout.addEditionCanvas(classEditionForm);
        mainFormLayout.addEditionCanvas(productionEditionForm);
        mainFormLayout.addEditionCanvas(diffusionEditionForm);
        mainFormLayout.addEditionCanvas(qualityEditionForm);
        mainFormLayout.addEditionCanvas(annotationsEditionForm);
    }

    private void setInstanceViewMode(InstanceDto instanceDto) {
        // Identifiers
        identifiersViewForm.setValue(InstanceDS.CODE, instanceDto.getCode());
        identifiersViewForm.setValue(InstanceDS.TITLE, RecordUtils.getInternationalStringRecord(instanceDto.getTitle()));
        identifiersViewForm.setValue(InstanceDS.ACRONYM, RecordUtils.getInternationalStringRecord(instanceDto.getAcronym()));

        // Content Classifiers

        // Content Descriptors
        contentViewForm.setValue(InstanceDS.DATA_DESCRIPTION, RecordUtils.getInternationalStringRecord(instanceDto.getDataDescription()));
        contentViewForm.setValue(InstanceDS.STATISTICAL_POPULATION, RecordUtils.getInternationalStringRecord(instanceDto.getStatisticalPopulation()));
        contentViewForm.setValue(InstanceDS.STATISTIAL_UNIT, ExternalItemUtils.getExternalItemListToString(instanceDto.getStatisticalUnit()));
        // FIXME contentViewForm
        // .setValue(InstanceDS.GEOGRAPHIC_GRANULARITY, instanceDto.getGeographicGranularity() != null ? ExternalItemUtils.getExternalItemName(instanceDto.getGeographicGranularity()) : "");
        contentViewForm.setValue(InstanceDS.GEOGRAPHIC_COMPARABILITY, RecordUtils.getInternationalStringRecord(instanceDto.getGeographicComparability()));
        // FIXME contentViewForm.setValue(InstanceDS.TEMPORAL_GRANULARITY, instanceDto.getTemporalGranularity() != null ? ExternalItemUtils.getExternalItemName(instanceDto.getTemporalGranularity()) :
        // "");
        contentViewForm.setValue(InstanceDS.TEMPORAL_COMPARABILITY, RecordUtils.getInternationalStringRecord(instanceDto.getTemporalComparability()));
        contentViewForm.setValue(InstanceDS.BASE_PERIOD, instanceDto.getBasePeriod());
        contentViewForm.setValue(InstanceDS.UNIT_MEASURE, ExternalItemUtils.getExternalItemListToString(instanceDto.getUnitMeasure()));
        contentViewForm.setValue(InstanceDS.STAT_CONC_DEF, RecordUtils.getInternationalStringRecord(instanceDto.getStatConcDef()));
        contentViewForm.setValue(InstanceDS.STAT_CONC_DEF_LIST, ExternalItemUtils.getExternalItemListToString(instanceDto.getStatConcDefList()));
        contentViewForm.setValue(InstanceDS.CLASS_SYSTEM, RecordUtils.getInternationalStringRecord(instanceDto.getClassSystem()));
        contentViewForm.setValue(InstanceDS.CLASS_SYSTEM_LIST, ExternalItemUtils.getExternalItemListToString(instanceDto.getClassSystemList()));

        // Class descriptors
        classViewForm.setValue(InstanceDS.INSTANCE_TYPE,
                instanceDto.getInstanceType() != null ? CommonWebUtils.getElementName(instanceDto.getInstanceType().getIdentifier(), instanceDto.getInstanceType().getDescription()) : "");

        // Production descriptors
        productionViewForm.setValue(InstanceDS.INTERNAL_INVENTORY_DATE, instanceDto.getInternalInventoryDate());
        productionViewForm.setValue(InstanceDS.PROC_STATUS, getCoreMessages().getString(getCoreMessages().procStatusEnum() + instanceDto.getProcStatus().getName()));

        productionViewForm.setValue(InstanceDS.DOC_METHOD, RecordUtils.getInternationalStringRecord(instanceDto.getDocMethod()));
        productionViewForm.setValue(InstanceDS.SURVEY_SOURCE,
                instanceDto.getSurveySource() != null ? CommonWebUtils.getElementName(instanceDto.getSurveySource().getIdentifier(), instanceDto.getSurveySource().getDescription()) : "");
        productionViewForm.setValue(InstanceDS.COLL_METHOD,
                instanceDto.getCollMethod() != null ? CommonWebUtils.getElementName(instanceDto.getCollMethod().getIdentifier(), instanceDto.getCollMethod().getDescription()) : "");
        productionViewForm.setValue(InstanceDS.INFORMATION_SUPPLIERS, ExternalItemUtils.getExternalItemListToString(instanceDto.getInformationSuppliers()));
        productionViewForm.setValue(InstanceDS.FREQ_COLL, ExternalItemUtils.getExternalItemListToString(instanceDto.getFreqColl()));

        productionViewForm.setValue(InstanceDS.DATA_VALIDATION, RecordUtils.getInternationalStringRecord(instanceDto.getDataValidation()));
        productionViewForm.setValue(InstanceDS.DATA_COMPILATION, RecordUtils.getInternationalStringRecord(instanceDto.getDataCompilation()));
        productionViewForm.setValue(InstanceDS.ADJUSTMENT, RecordUtils.getInternationalStringRecord(instanceDto.getAdjustment()));
        productionViewForm.setValue(InstanceDS.COST_BURDEN, RecordUtils.getInternationalStringRecord(instanceDto.getCostBurden()));

        productionViewForm.setValue(InstanceDS.COST, OperationsListUtils.getCostDtoListToString(instanceDto.getCost()));

        // Diffusion and Publication
        diffusionViewForm.setValue(InstanceDS.INVENTORY_DATE, instanceDto.getInventoryDate());

        // Quality Descriptors
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

        // Annotations
        annotationsViewForm.setValue(InstanceDS.COMMENTS, RecordUtils.getInternationalStringRecord(instanceDto.getComment()));
        annotationsViewForm.setValue(InstanceDS.NOTES, RecordUtils.getInternationalStringRecord(instanceDto.getNotes()));
    }
    private void setInstanceEditionMode(InstanceDto instanceDto) {
        // Identifiers
        code.setValue(instanceDto.getCode());
        identifiersEditionForm.setValue(InstanceDS.CODE_VIEW, instanceDto.getCode());

        title.setValue(instanceDto.getTitle());
        acronym.setValue(instanceDto.getAcronym());

        // Content classifiers

        // Content descriptors
        dataDescriptionItem.setValue(instanceDto.getDataDescription());
        statisticalPopulationItem.setValue(instanceDto.getStatisticalPopulation());
        statisticalUnitItem.clearValue();
        // FIXME geographicalGranularityItem.setValue(instanceDto.getGeographicGranularity() != null ? instanceDto.getGeographicGranularity().getUrn() : "");
        geographicalComparabilityItem.setValue(instanceDto.getGeographicComparability());
        // FIXME temporalGranularityItem.setValue(instanceDto.getTemporalGranularity() != null ? instanceDto.getTemporalGranularity().getUrn() : "");
        temporalComparabilityItem.setValue(instanceDto.getTemporalComparability());
        contentEditionForm.setValue(InstanceDS.BASE_PERIOD, instanceDto.getBasePeriod());
        unitMeasureItem.setValues(ExternalItemUtils.getExternalItemsUrns(instanceDto.getUnitMeasure()));
        statConcDefItem.setValue(instanceDto.getStatConcDef());
        statConcDefListItem.setValues(ExternalItemUtils.getExternalItemsUrns(instanceDto.getStatConcDefList()));
        classSystemItem.setValue(instanceDto.getClassSystem());
        classSystemListItem.setValues(ExternalItemUtils.getExternalItemsUrns(instanceDto.getClassSystemList()));

        // Class descriptors
        instanceTypeItem.setValue(instanceDto.getInstanceType() != null ? instanceDto.getInstanceType().getId().toString() : "");

        // Production descriptors
        productionEditionForm.setValue(InstanceDS.INTERNAL_INVENTORY_DATE, instanceDto.getInventoryDate());

        productionEditionForm.setValue(InstanceDS.PROC_STATUS, getCoreMessages().getString(getCoreMessages().procStatusEnum() + instanceDto.getProcStatus().getName()));
        productionEditionForm.setValue(InstanceDS.PROC_STATUS_VIEW, instanceDto.getProcStatus().getName());

        productionEditionForm.setValue(InstanceDS.DOC_METHOD, RecordUtils.getInternationalStringRecord(instanceDto.getDocMethod()));
        surveySourceItem.setValue(instanceDto.getSurveySource() != null ? instanceDto.getSurveySource().getId() : "");
        collMethodItem.setValue(instanceDto.getCollMethod() != null ? instanceDto.getCollMethod().getId() : "");
        infSuppliersConceptsItem.clearValue();
        infSuppliersConceptsItem.clearValue();
        freqCollItem.setValues(ExternalItemUtils.getExternalItemsUrns(instanceDto.getFreqColl()));
        productionEditionForm.setValue(InstanceDS.DATA_VALIDATION, RecordUtils.getInternationalStringRecord(instanceDto.getDataValidation()));
        productionEditionForm.setValue(InstanceDS.DATA_COMPILATION, RecordUtils.getInternationalStringRecord(instanceDto.getDataCompilation()));
        productionEditionForm.setValue(InstanceDS.ADJUSTMENT, RecordUtils.getInternationalStringRecord(instanceDto.getAdjustment()));
        productionEditionForm.setValue(InstanceDS.COST_BURDEN, RecordUtils.getInternationalStringRecord(instanceDto.getCostBurden()));
        costItem.setValues(getCostIds(instanceDto.getCost()));

        // Diffusion and Publication
        diffusionEditionForm.setValue(InstanceDS.INVENTORY_DATE, instanceDto.getInventoryDate());

        // Quality Descriptors
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

        // Annotations
        annotationsEditionForm.setValue(InstanceDS.COMMENTS, RecordUtils.getInternationalStringRecord(instanceDto.getComment()));
        annotationsEditionForm.setValue(InstanceDS.NOTES, RecordUtils.getInternationalStringRecord(instanceDto.getNotes()));

        identifiersEditionForm.markForRedraw();
        productionEditionForm.markForRedraw();
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

    @Override
    public void setOrganisationScheme(List<ExternalItemDto> schemes) {
        infSuppliersOrganItem.setSchemesValueMap(ExternalItemUtils.getExternalItemsHashMap(schemes));
    }

    @Override
    public void setConceptScheme(List<ExternalItemDto> schemes) {
        this.conceptSchemes = schemes;
        LinkedHashMap<String, String> map = ExternalItemUtils.getExternalItemsHashMap(schemes);
        statisticalUnitItem.setSchemesValueMap(map);
        infSuppliersConceptsItem.setSchemesValueMap(map);
        statConcDefListItem.setValueMap(map);
    }

    @Override
    public void setInfSuppliersOrg(List<ExternalItemDto> organisations) {
        this.infSuppliersOrganisations = organisations;
        infSuppliersOrganItem.setItemsValueMap(ExternalItemUtils.getExternalItemsHashMap(organisations));
    }

    @Override
    public void setInfSuppliersConcept(List<ExternalItemDto> concepts) {
        this.infSuppliersConcepts = concepts;
        infSuppliersConceptsItem.setItemsValueMap(ExternalItemUtils.getExternalItemsHashMap(concepts));
    }

    @Override
    public void setStatisticalUnitConcepts(List<ExternalItemDto> concepts) {
        this.statisticalUnitConcepts = concepts;
        statisticalUnitItem.setItemsValueMap(ExternalItemUtils.getExternalItemsHashMap(concepts));
    }

    @Override
    public void setCodeLists(List<ExternalItemDto> codeLists) {
        this.codeLists = codeLists;
        LinkedHashMap<String, String> map = ExternalItemUtils.getExternalItemsHashMap(codeLists);
        geographicalGranularityItem.setValueMap(map);
        unitMeasureItem.setValueMap(map);
        classSystemListItem.setValueMap(map);
    }

    @Override
    public void setTemporalGranularityCodes(List<ExternalItemDto> codes) {
        this.temporalGranularityCodes = codes;
        temporalGranularityItem.setValueMap(ExternalItemUtils.getExternalItemsHashMap(codes));
    }

    @Override
    public void setFreqCollCodes(List<ExternalItemDto> codes) {
        this.freqCollCodes = codes;
        freqCollItem.setValueMap(ExternalItemUtils.getExternalItemsHashMap(codes));
    }

    private void setTranslationsShowed(boolean translationsShowed) {
        // Set translationsShowed value to international fields
        identifiersViewForm.setTranslationsShowed(translationsShowed);
        identifiersEditionForm.setTranslationsShowed(translationsShowed);
        contentViewForm.setTranslationsShowed(translationsShowed);
        contentEditionForm.setTranslationsShowed(translationsShowed);
        productionViewForm.setTranslationsShowed(translationsShowed);
        productionEditionForm.setTranslationsShowed(translationsShowed);
        qualityViewForm.setTranslationsShowed(translationsShowed);
        qualityEditionForm.setTranslationsShowed(translationsShowed);
        annotationsViewForm.setTranslationsShowed(translationsShowed);
        annotationsEditionForm.setTranslationsShowed(translationsShowed);
    }

    private boolean canInstanceCodeBeEdited() {
        // Operation code can be edited only when ProcStatus is DRAFT
        return (productionEditionForm.getValue(InstanceDS.PROC_STATUS_VIEW) != null && ProcStatusEnum.DRAFT.toString().equals(productionEditionForm.getValue(InstanceDS.PROC_STATUS_VIEW)));
    }

    /**
     * Validate those metadata that are required is instance status is PUBLISH_INTERNALLY (if status is PUBLISH_EXTERNALLY, metadata should be required too)
     * 
     * @return
     */
    // private RequiredIfValidator getRequiredIfInternallyPublished() {
    // return new RequiredIfValidator(new RequiredIfFunction() {
    //
    // @Override
    // public boolean execute(FormItem formItem, Object value) {
    // return isInstanceInternallyPublished() || isInstanceExternallyPublished();
    // }
    // });
    // }

    public boolean isInstanceInternallyPublished() {
        return ProcStatusEnum.PUBLISH_INTERNALLY.equals(instanceDto.getProcStatus());
    }

    public boolean isInstanceExternallyPublished() {
        return ProcStatusEnum.PUBLISH_EXTERNALLY.equals(instanceDto.getProcStatus());
    }

}
