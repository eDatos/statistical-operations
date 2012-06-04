package org.siemac.metamac.statistical.operations.web.client.instance.view;

import static org.siemac.metamac.statistical.operations.web.client.OperationsWeb.getConstants;
import static org.siemac.metamac.statistical.operations.web.client.OperationsWeb.getCoreMessages;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Set;

import org.siemac.metamac.core.common.dto.ExternalItemBtDto;
import org.siemac.metamac.domain.statistical.operations.dto.CollMethodDto;
import org.siemac.metamac.domain.statistical.operations.dto.CostDto;
import org.siemac.metamac.domain.statistical.operations.dto.InstanceDto;
import org.siemac.metamac.domain.statistical.operations.dto.InstanceTypeDto;
import org.siemac.metamac.domain.statistical.operations.dto.SurveySourceDto;
import org.siemac.metamac.statistical.operations.web.client.instance.presenter.InstancePresenter;
import org.siemac.metamac.statistical.operations.web.client.instance.view.handlers.InstanceUiHandlers;
import org.siemac.metamac.statistical.operations.web.client.utils.OperationsListUtils;
import org.siemac.metamac.statistical.operations.web.client.widgets.InstanceMainFormLayout;
import org.siemac.metamac.web.common.client.utils.ExternalItemUtils;
import org.siemac.metamac.web.common.client.utils.InternationalStringUtils;
import org.siemac.metamac.web.common.client.utils.RecordUtils;
import org.siemac.metamac.web.common.client.utils.TimeVariableWebUtils;
import org.siemac.metamac.web.common.client.widgets.form.GroupDynamicForm;
import org.siemac.metamac.web.common.client.widgets.form.fields.ExternalMultipleSelectItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.MultiLanguageTextAndUrlItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.MultiLanguageTextItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.RequiredTextItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.ViewMultiLanguageTextAndUrlItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.ViewMultiLanguageTextItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.ViewTextItem;

import com.google.gwt.user.client.ui.Widget;
import com.gwtplatform.mvp.client.ViewWithUiHandlers;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.events.HasClickHandlers;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.VLayout;

public class InstanceViewImpl extends ViewWithUiHandlers<InstanceUiHandlers> implements InstancePresenter.InstanceView {

    // IDENTIFIERS
    private static final String             IN_IDENTIFIER               = "op-id";
    private static final String             IN_TITLE                    = "op-title";
    private static final String             IN_ACRONYM                  = "op-acron";
    // CONTENT CLASSIFIERS

    // CONTENT DESCRIPTORS
    private static final String             IN_DATA_DESCRIPTION         = "in-ddesc";
    private static final String             IN_STATISTICAL_POPULATION   = "in-sta-pop";
    private static final String             IN_STATISTIAL_UNIT          = "in-sta-unit";
    private static final String             IN_GEOGRAPHIC_GRANULARITY   = "in-geo-gran";
    private static final String             IN_GEOGRAPHIC_COMPARABILITY = "in-geo-com";
    private static final String             IN_TEMPORAL_GRANULARITY     = "in-tem-gran";
    private static final String             IN_TEMPORAL_COMPARABILITY   = "in-tem-com";
    private static final String             IN_BASE_PERIOD              = "in-basep";
    private static final String             IN_UNIT_MEASURE             = "in-umeas";
    private static final String             IN_STAT_CONC_DEF            = "in-sta-con";
    private static final String             IN_STAT_CONC_DEF_LIST       = "in-sta-con-list";
    private static final String             IN_CLASS_SYSTEM             = "in-class-sys";
    private static final String             IN_CLASS_SYSTEM_LIST        = "in-class-sys-list";
    // CLASS DESCRIPTORS
    private static final String             IN_INSTANCE_TYPE            = "in-intype";
    // PRODUCTION DESCRIPTORS
    private static final String             IN_INTERNAL_INVENTORY_DATE  = "in-int-inv-date";
    private static final String             IN_PROC_STATUS              = "in-proc-status";
    private static final String             IN_DOC_METHOD               = "in-doc-met";
    private static final String             IN_SURVEY_SOURCE            = "in-ssource";
    private static final String             IN_COLL_METHOD              = "in-coll-met";
    private static final String             IN_INFORMATION_SUPPLIERS    = "in-suppliers";
    private static final String             IN_FREQ_COLL                = "in-freq-coll";
    private static final String             IN_DATA_VALIDATION          = "in-valid";
    private static final String             IN_DATA_COMPILATION         = "in-compil";
    private static final String             IN_ADJUSTMENT               = "in-adjust";
    private static final String             IN_COST_BURDEN              = "in-cost-burden";
    private static final String             IN_COST                     = "in-cost";
    // DIFFUSION DESCRIPTORS
    private static final String             IN_INVENTORY_DATE           = "in-indate";
    // QUALITY DESCRIPTORS
    private static final String             IN_QUALITY_DOC              = "in-qdoc";
    private static final String             IN_QUALITY_ASSURE           = "in-qassu";
    private static final String             IN_QUALITY_ASSMNT           = "in-qassm";
    private static final String             IN_USER_NEEDS               = "in-usern";
    private static final String             IN_USER_SAT                 = "in-users";
    private static final String             IN_COMPLETENESS             = "in-comp";
    private static final String             IN_TIMELINESS               = "in-tim";
    private static final String             IN_PUNCTUALITY              = "in-punc";
    private static final String             IN_ACCURACY_OVERALL         = "in-accu";
    private static final String             IN_SAMPLING_ERROR           = "in-samp";
    private static final String             IN_NONSAMPLING_ERR          = "in-nons";
    private static final String             IN_COHER_X_DOM              = "in-coher";
    private static final String             IN_COHER_INTERNAL           = "in-intl";
    // ANNOTATIONS
    private static final String             IN_COMMENTS                 = "op-com";
    private static final String             IN_NOTES                    = "op-not";

    private VLayout                         panel;

    private InstanceMainFormLayout           mainFormLayout;
    // IDENTIFIERS
    private GroupDynamicForm                identifiersViewForm;
    private GroupDynamicForm                identifiersEditionForm;
    private RequiredTextItem                identifier;
    private MultiLanguageTextItem           title;
    private MultiLanguageTextItem           acronym;

    // CONTENT DESCRIPTORS
    private GroupDynamicForm                contentViewForm;
    private GroupDynamicForm                contentEditionForm;
    private MultiLanguageTextItem           dataDescriptionItem;
    private MultiLanguageTextItem           statisticalPopulationItem;
    private ExternalMultipleSelectItem      statisticalUnitItem;
    private SelectItem                      geographicalGranularityItem;
    private MultiLanguageTextItem           geographicalComparabilityItem;
    private SelectItem                      temporalGranularityItem;
    private MultiLanguageTextItem           temporalComparabilityItem;
    // TODO base period
    private SelectItem                      unitMeasureItem;
    private MultiLanguageTextItem           statConcDefItem;
    private SelectItem                      statConcDefListItem;
    private MultiLanguageTextItem           classSystemItem;
    private SelectItem                      classSystemListItem;

    // CLASS DESCRIPTORS
    private GroupDynamicForm                classViewForm;
    private GroupDynamicForm                classEditionForm;
    private SelectItem                      instanceTypeItem;

    // PRODUCTION DESCRIPTORS
    private GroupDynamicForm                productionViewForm;
    private GroupDynamicForm                productionEditionForm;
    private MultiLanguageTextAndUrlItem     docMethodItem;
    private SelectItem                      collMethodItem;
    private SelectItem                      surveySourceItem;
    private ExternalMultipleSelectItem      infSuppliersOrganItem;
    private ExternalMultipleSelectItem      infSuppliersConceptsItem;
    private SelectItem                      freqCollItem;
    private MultiLanguageTextAndUrlItem     dataValidationItem;
    private MultiLanguageTextAndUrlItem     dataCompilationItem;
    private MultiLanguageTextAndUrlItem     adjustmentItem;
    private MultiLanguageTextAndUrlItem     costBurdenItem;
    private SelectItem                      costItem;

    private ViewMultiLanguageTextAndUrlItem staticDocMethodItem;
    private ViewMultiLanguageTextAndUrlItem staticDataValidationItem;
    private ViewMultiLanguageTextAndUrlItem staticDataCompilationItem;
    private ViewMultiLanguageTextAndUrlItem staticAdjustmentItem;
    private ViewMultiLanguageTextAndUrlItem staticCostBurdenItem;

    // DIFFUSION AND PUBLICATION
    private GroupDynamicForm                diffusionViewForm;
    private GroupDynamicForm                diffusionEditionForm;

    // QUALITY DESCRIPTORS
    private GroupDynamicForm                qualityViewForm;
    private GroupDynamicForm                qualityEditionForm;
    private MultiLanguageTextAndUrlItem     qualityDocItem;
    private MultiLanguageTextAndUrlItem     qualityAssureItem;
    private MultiLanguageTextAndUrlItem     qualityAssesmentItem;
    private MultiLanguageTextAndUrlItem     userNeedsItem;
    private MultiLanguageTextAndUrlItem     userSatItem;
    private MultiLanguageTextAndUrlItem     completenessItem;
    private MultiLanguageTextAndUrlItem     timelinessItem;
    private MultiLanguageTextAndUrlItem     punctualityItem;
    private MultiLanguageTextAndUrlItem     accuracyOverallItem;
    private MultiLanguageTextAndUrlItem     samplingErrItem;
    private MultiLanguageTextAndUrlItem     nonSamplingErrItem;
    private MultiLanguageTextAndUrlItem     coherXDomItem;
    private MultiLanguageTextAndUrlItem     coherInternalItem;

    private ViewMultiLanguageTextAndUrlItem staticQualityDocItem;
    private ViewMultiLanguageTextAndUrlItem staticQualityAssureItem;
    private ViewMultiLanguageTextAndUrlItem staticQualityAssesmentItem;
    private ViewMultiLanguageTextAndUrlItem staticUserNeedsItem;
    private ViewMultiLanguageTextAndUrlItem staticUserSatItem;
    private ViewMultiLanguageTextAndUrlItem staticCompletenessItem;
    private ViewMultiLanguageTextAndUrlItem staticTimelinessItem;
    private ViewMultiLanguageTextAndUrlItem staticPunctualityItem;
    private ViewMultiLanguageTextAndUrlItem staticAccuracyOverallItem;
    private ViewMultiLanguageTextAndUrlItem staticSamplingErrItem;
    private ViewMultiLanguageTextAndUrlItem statocNonSamplingErrItem;
    private ViewMultiLanguageTextAndUrlItem staticCoherXDomItem;
    private ViewMultiLanguageTextAndUrlItem staticCoherInternalItem;

    // ANNOTATIONS
    private GroupDynamicForm                annotationsViewForm;
    private GroupDynamicForm                annotationsEditionForm;
    private MultiLanguageTextAndUrlItem     commentItem;
    private MultiLanguageTextAndUrlItem     notesItem;

    private ViewMultiLanguageTextAndUrlItem staticCommentItem;
    private ViewMultiLanguageTextAndUrlItem staticNotesItem;

    private List<ExternalItemBtDto>         conceptSchemes;
    private List<ExternalItemBtDto>         codeLists;

    private List<ExternalItemBtDto>         statisticalUnitConcepts;
    private List<ExternalItemBtDto>         infSuppliersOrganisations;
    private List<ExternalItemBtDto>         infSuppliersConcepts;
    private List<ExternalItemBtDto>         temporalGranularityCodes;
    private List<ExternalItemBtDto>         freqCollCodes;

    private List<InstanceTypeDto>           instanceTypeDtos;
    private List<SurveySourceDto>           surveySourceDtos;
    private List<CollMethodDto>             collMethodDtos;
    private List<CostDto>                   costDtos;

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
                panel.addMember(content, 0);
            }
        } else {
            // To support inheritance in your views it is good practice to call super.setInSlot when you can't handle the call.
            // Who knows, maybe the parent class knows what to do with this slot.
            super.setInSlot(slot, content);
        }
    }

    @Override
    public void setInstance(InstanceDto instanceDto) {
        mainFormLayout.setViewMode();
        mainFormLayout.updatePublishSection(instanceDto.getProcStatus());
        // Set Instance
        mainFormLayout.setTitleLabelContents(InternationalStringUtils.getLocalisedString(instanceDto.getTitle()));
        setViewForm(instanceDto);
        setEditionForm(instanceDto);
    }

    @Override
    public HasClickHandlers getSave() {
        return mainFormLayout.getSave();
    }

    @Override
    public void onInstanceSaved(InstanceDto instanceDto) {
        setInstance(instanceDto);
    }

    @Override
    public InstanceDto getInstance(InstanceDto instanceDto) {
        // Identifiers
        instanceDto.setCode(instanceDto.getCode());
        instanceDto.setTitle(title.getValue());
        instanceDto.setAcronym(acronym.getValue());

        // Content Classifiers

        // Content Descriptors
        instanceDto.setDataDescription(dataDescriptionItem.getValue());
        instanceDto.setStatisticalPopulation(statisticalPopulationItem.getValue());
        instanceDto.getStatisticalUnit().clear();
        instanceDto.getStatisticalUnit().addAll(statisticalUnitItem.getSelectedExternalItems(statisticalUnitConcepts));
        instanceDto.setGeographicGranularity(ExternalItemUtils.getExternalItemBtDtoFromCodeId(codeLists, geographicalGranularityItem.getValueAsString()));
        instanceDto.setGeographicComparability(geographicalComparabilityItem.getValue());
        instanceDto.setTemporalGranularity(ExternalItemUtils.getExternalItemBtDtoFromCodeId(temporalGranularityCodes, temporalGranularityItem.getValueAsString()));
        instanceDto.setTemporalComparability(temporalComparabilityItem.getValue());
        // TODO Base period
        instanceDto.getUnitMeasure().clear();
        instanceDto.getUnitMeasure().addAll(ExternalItemUtils.getExternalItemBtDtoListFromCodeIds(codeLists, unitMeasureItem.getValues()));
        instanceDto.setStatConcDef(statConcDefItem.getValue());
        instanceDto.getStatConcDefList().clear();
        instanceDto.getStatConcDefList().addAll(ExternalItemUtils.getExternalItemBtDtoListFromCodeIds(conceptSchemes, statConcDefListItem.getValues()));
        instanceDto.setClassSystem(classSystemItem.getValue());
        instanceDto.getClassSystemList().clear();
        instanceDto.getClassSystemList().addAll(ExternalItemUtils.getExternalItemBtDtoListFromCodeIds(codeLists, classSystemListItem.getValues()));

        // Class descriptors
        instanceDto.setInstanceType(OperationsListUtils.getInstanceTypeDto(instanceTypeItem.getValueAsString(), instanceTypeDtos));

        // Production descriptors
        instanceDto.setDocMethod(docMethodItem.getTextValue());
        instanceDto.setDocMethodUrl(docMethodItem.getUrlValue());
        instanceDto.setSurveySource(OperationsListUtils.getSurveySourceDto(surveySourceItem.getValueAsString(), surveySourceDtos));
        instanceDto.setCollMethod(OperationsListUtils.getCollMethodDto(collMethodItem.getValueAsString(), collMethodDtos));
        instanceDto.getInformationSuppliers().clear();
        instanceDto.getInformationSuppliers().addAll(infSuppliersOrganItem.getSelectedExternalItems(infSuppliersOrganisations));
        instanceDto.getInformationSuppliers().addAll(infSuppliersConceptsItem.getSelectedExternalItems(infSuppliersConcepts));
        instanceDto.getFreqColl().clear();
        instanceDto.getFreqColl().addAll(ExternalItemUtils.getExternalItemBtDtoListFromCodeIds(freqCollCodes, freqCollItem.getValues()));
        instanceDto.setDataValidation(dataValidationItem.getTextValue());
        instanceDto.setDataValidationUrl(dataValidationItem.getUrlValue());
        instanceDto.setDataCompilation(dataCompilationItem.getTextValue());
        instanceDto.setDataCompilationUrl(dataCompilationItem.getUrlValue());
        instanceDto.setAdjustment(adjustmentItem.getTextValue());
        instanceDto.setAdjustmentUrl(adjustmentItem.getUrlValue());
        instanceDto.setCostBurden(costBurdenItem.getTextValue());
        instanceDto.setCostBurdenUrl(costBurdenItem.getUrlValue());
        instanceDto.getCost().clear();
        instanceDto.getCost().addAll(OperationsListUtils.getCostDtos(costItem.getValues(), costDtos));

        // QUALITY DESCRIPTORS
        instanceDto.setQualityDoc(qualityDocItem.getTextValue());
        instanceDto.setQualityDocUrl(qualityDocItem.getUrlValue());
        instanceDto.setQualityAssure(qualityAssureItem.getTextValue());
        instanceDto.setQualityAssureUrl(qualityAssureItem.getUrlValue());
        instanceDto.setQualityAssmnt(qualityAssesmentItem.getTextValue());
        instanceDto.setQualityAssmntUrl(qualityAssesmentItem.getUrlValue());
        instanceDto.setUserNeeds(userNeedsItem.getTextValue());
        instanceDto.setUserNeedsUrl(userNeedsItem.getUrlValue());
        instanceDto.setUserSat(userSatItem.getTextValue());
        instanceDto.setUserSatUrl(userSatItem.getUrlValue());
        instanceDto.setCompleteness(completenessItem.getTextValue());
        instanceDto.setCompletenessUrl(completenessItem.getUrlValue());
        instanceDto.setTimeliness(timelinessItem.getTextValue());
        instanceDto.setTimelinessUrl(timelinessItem.getUrlValue());
        instanceDto.setPunctuality(punctualityItem.getTextValue());
        instanceDto.setPunctualityUrl(punctualityItem.getUrlValue());
        instanceDto.setAccuracyOverall(accuracyOverallItem.getTextValue());
        instanceDto.setAccuracyOverallUrl(accuracyOverallItem.getUrlValue());
        instanceDto.setSamplingErr(samplingErrItem.getTextValue());
        instanceDto.setSamplingErrUrl(samplingErrItem.getUrlValue());
        instanceDto.setNonsamplingErr(nonSamplingErrItem.getTextValue());
        instanceDto.setNonsamplingErrUrl(nonSamplingErrItem.getUrlValue());
        instanceDto.setCoherXDomain(coherXDomItem.getTextValue());
        instanceDto.setCoherXDomainUrl(coherXDomItem.getUrlValue());
        instanceDto.setCoherInternal(coherInternalItem.getTextValue());
        instanceDto.setCoherInternalUrl(coherInternalItem.getUrlValue());

        // ANNOTATIONS
        instanceDto.setComment(commentItem.getTextValue());
        instanceDto.setCommentUrl(commentItem.getUrlValue());
        instanceDto.setNotes(notesItem.getTextValue());
        instanceDto.setNotesUrl(commentItem.getUrlValue());

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
        ViewTextItem identifier = new ViewTextItem(IN_IDENTIFIER, getConstants().instanceIdentifier());
        ViewMultiLanguageTextItem title = new ViewMultiLanguageTextItem(IN_TITLE, getConstants().instanceTitle());
        ViewMultiLanguageTextItem alternativeTitle = new ViewMultiLanguageTextItem(IN_ACRONYM, getConstants().instanceAcronym());
        identifiersViewForm.setFields(identifier, title, alternativeTitle);

        // Content classifiers

        // Content descriptors
        contentViewForm = new GroupDynamicForm(getConstants().instanceContentDescriptors());
        ViewMultiLanguageTextItem dataDescription = new ViewMultiLanguageTextItem(IN_DATA_DESCRIPTION, getConstants().instanceDataDescription());
        ViewMultiLanguageTextItem statisticalPopulation = new ViewMultiLanguageTextItem(IN_STATISTICAL_POPULATION, getConstants().instanceStatisticalPopulation());
        ViewTextItem statisticalUnit = new ViewTextItem(IN_STATISTIAL_UNIT, getConstants().instanceStatisticaUnit());
        ViewTextItem geographicGranularity = new ViewTextItem(IN_GEOGRAPHIC_GRANULARITY, getConstants().instanceGeographicGranularity());
        ViewMultiLanguageTextItem geographicComparability = new ViewMultiLanguageTextItem(IN_GEOGRAPHIC_COMPARABILITY, getConstants().instanceGeographicComparability());
        ViewTextItem temporalGranularity = new ViewTextItem(IN_TEMPORAL_GRANULARITY, getConstants().instanceTemporalGranularity());
        ViewMultiLanguageTextItem temporalComparability = new ViewMultiLanguageTextItem(IN_TEMPORAL_COMPARABILITY, getConstants().instanceTemporalComparability());
        ViewTextItem basePeriodItem = new ViewTextItem(IN_BASE_PERIOD, getConstants().instanceBasePeriod());
        ViewTextItem unitMeasure = new ViewTextItem(IN_UNIT_MEASURE, getConstants().instanceUnitMeasure());
        ViewMultiLanguageTextItem statConcDef = new ViewMultiLanguageTextItem(IN_STAT_CONC_DEF, getConstants().instanceStatisticalConceptDefinition());
        ViewTextItem statConcDefList = new ViewTextItem(IN_STAT_CONC_DEF_LIST, getConstants().instanceStatisticalConceptsDefinitions());
        ViewMultiLanguageTextItem classSystem = new ViewMultiLanguageTextItem(IN_CLASS_SYSTEM, getConstants().instanceClassSystem());
        ViewTextItem classSystemList = new ViewTextItem(IN_CLASS_SYSTEM_LIST, getConstants().instanceClassSystemList());
        contentViewForm.setFields(dataDescription, statisticalPopulation, statisticalUnit, geographicGranularity, geographicComparability, temporalGranularity, temporalComparability, basePeriodItem,
                unitMeasure, statConcDef, statConcDefList, classSystem, classSystemList);

        // Class descriptors
        classViewForm = new GroupDynamicForm(getConstants().instanceClassDescriptors());
        ViewTextItem instanceType = new ViewTextItem(IN_INSTANCE_TYPE, getConstants().instanceType());
        classViewForm.setFields(instanceType);

        // Production descriptors
        productionViewForm = new GroupDynamicForm(getConstants().instanceProductionDescriptors());
        ViewTextItem internalInventoryDate = new ViewTextItem(IN_INTERNAL_INVENTORY_DATE, getConstants().instanceInternalInventoryDate());
        ViewTextItem procStatus = new ViewTextItem(IN_PROC_STATUS, getConstants().instanceProcStatus());
        staticDocMethodItem = new ViewMultiLanguageTextAndUrlItem(IN_DOC_METHOD, getConstants().instanceDocMethod());
        ViewTextItem surveySource = new ViewTextItem(IN_SURVEY_SOURCE, getConstants().instanceSurveySource());
        ViewTextItem collMethod = new ViewTextItem(IN_COLL_METHOD, getConstants().instanceCollMethod());
        ViewTextItem informationSuppliers = new ViewTextItem(IN_INFORMATION_SUPPLIERS, getConstants().instanceInformationSuppliers());
        ViewTextItem freqColl = new ViewTextItem(IN_FREQ_COLL, getConstants().instanceFreqColl());
        staticDataValidationItem = new ViewMultiLanguageTextAndUrlItem(IN_DATA_VALIDATION, getConstants().instanceDataValidation());
        staticDataCompilationItem = new ViewMultiLanguageTextAndUrlItem(IN_DATA_COMPILATION, getConstants().instanceDataCompilation());
        staticAdjustmentItem = new ViewMultiLanguageTextAndUrlItem(IN_ADJUSTMENT, getConstants().instanceAdjustment());
        staticCostBurdenItem = new ViewMultiLanguageTextAndUrlItem(IN_COST_BURDEN, getConstants().instanceCostBurden());
        ViewTextItem cost = new ViewTextItem(IN_COST, getConstants().instanceCost());
        productionViewForm.setFields(internalInventoryDate, procStatus, staticDocMethodItem, surveySource, collMethod, informationSuppliers, freqColl, staticDataValidationItem,
                staticDataCompilationItem, staticAdjustmentItem, staticCostBurdenItem, cost);

        // Diffusion and Publication
        diffusionViewForm = new GroupDynamicForm(getConstants().instanceDiffusionDescriptors());
        ViewTextItem inventoryDate = new ViewTextItem(IN_INVENTORY_DATE, getConstants().instanceInventoryDate());
        diffusionViewForm.setFields(inventoryDate);

        // Quality descriptors
        qualityViewForm = new GroupDynamicForm(getConstants().instanceQualityDescriptors());
        staticQualityDocItem = new ViewMultiLanguageTextAndUrlItem(IN_QUALITY_DOC, getConstants().instanceQDoc());
        staticQualityAssureItem = new ViewMultiLanguageTextAndUrlItem(IN_QUALITY_ASSURE, getConstants().instanceQAssure());
        staticQualityAssesmentItem = new ViewMultiLanguageTextAndUrlItem(IN_QUALITY_ASSMNT, getConstants().instanceQAssmnt());
        staticUserNeedsItem = new ViewMultiLanguageTextAndUrlItem(IN_USER_NEEDS, getConstants().instanceUserNeeds());
        staticUserSatItem = new ViewMultiLanguageTextAndUrlItem(IN_USER_SAT, getConstants().instanceUserSat());
        staticCompletenessItem = new ViewMultiLanguageTextAndUrlItem(IN_COMPLETENESS, getConstants().instanceCompleteness());
        staticTimelinessItem = new ViewMultiLanguageTextAndUrlItem(IN_TIMELINESS, getConstants().instanceTimeliness());
        staticPunctualityItem = new ViewMultiLanguageTextAndUrlItem(IN_PUNCTUALITY, getConstants().instancePunctuality());
        staticAccuracyOverallItem = new ViewMultiLanguageTextAndUrlItem(IN_ACCURACY_OVERALL, getConstants().instanceAccuracyOverall());
        staticSamplingErrItem = new ViewMultiLanguageTextAndUrlItem(IN_SAMPLING_ERROR, getConstants().instanceSamplingErr());
        statocNonSamplingErrItem = new ViewMultiLanguageTextAndUrlItem(IN_NONSAMPLING_ERR, getConstants().instanceNonSamplingErr());
        staticCoherXDomItem = new ViewMultiLanguageTextAndUrlItem(IN_COHER_X_DOM, getConstants().instanceCoherXDom());
        staticCoherInternalItem = new ViewMultiLanguageTextAndUrlItem(IN_COHER_INTERNAL, getConstants().instanceCoherInter());
        qualityViewForm.setFields(staticQualityDocItem, staticQualityAssureItem, staticQualityAssesmentItem, staticUserNeedsItem, staticUserSatItem, staticCompletenessItem, staticTimelinessItem,
                staticPunctualityItem, staticAccuracyOverallItem, staticSamplingErrItem, statocNonSamplingErrItem, staticCoherXDomItem, staticCoherInternalItem);

        // Annotations
        annotationsViewForm = new GroupDynamicForm(getConstants().instanceAnnotations());
        staticCommentItem = new ViewMultiLanguageTextAndUrlItem(IN_COMMENTS, getConstants().instanceComments());
        staticNotesItem = new ViewMultiLanguageTextAndUrlItem(IN_NOTES, getConstants().instanceNotes());
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
        identifier = new RequiredTextItem(IN_IDENTIFIER, getConstants().instanceIdentifier());
        title = new MultiLanguageTextItem(IN_TITLE, getConstants().instanceTitle());
        title.setRequired(true);
        acronym = new MultiLanguageTextItem(IN_ACRONYM, getConstants().instanceAcronym());
        identifiersEditionForm.setFields(identifier, title, acronym);

        // Content classifiers

        // Content descriptors
        contentEditionForm = new GroupDynamicForm(getConstants().instanceContentDescriptors());
        dataDescriptionItem = new MultiLanguageTextItem(IN_DATA_DESCRIPTION, getConstants().instanceDataDescription());
        statisticalPopulationItem = new MultiLanguageTextItem(IN_STATISTICAL_POPULATION, getConstants().instanceStatisticalPopulation());
        statisticalUnitItem = new ExternalMultipleSelectItem(IN_STATISTIAL_UNIT, getConstants().instanceStatisticaUnit());
        statisticalUnitItem.getSchemeItem().addChangedHandler(new ChangedHandler() {

            @Override
            public void onChanged(ChangedEvent event) {
                if (event.getValue() != null) {
                    getUiHandlers().populateStatisticalUnitConcepts(event.getValue().toString());
                }
            }
        });
        geographicalGranularityItem = new SelectItem(IN_GEOGRAPHIC_GRANULARITY, getConstants().instanceGeographicGranularity());
        geographicalComparabilityItem = new MultiLanguageTextItem(IN_GEOGRAPHIC_COMPARABILITY, getConstants().instanceGeographicComparability());
        temporalGranularityItem = new SelectItem(IN_TEMPORAL_GRANULARITY, getConstants().instanceTemporalGranularity());
        temporalComparabilityItem = new MultiLanguageTextItem(IN_TEMPORAL_COMPARABILITY, getConstants().instanceTemporalComparability());
        TextItem basePeriodItem = new TextItem(IN_BASE_PERIOD, getConstants().instanceBasePeriod());
        basePeriodItem.setValidators(TimeVariableWebUtils.getTimeCustomValidator());
        unitMeasureItem = new SelectItem(IN_UNIT_MEASURE, getConstants().instanceUnitMeasure());
        unitMeasureItem.setMultiple(true);
        statConcDefItem = new MultiLanguageTextItem(IN_STAT_CONC_DEF, getConstants().instanceStatisticalConceptDefinition());
        statConcDefListItem = new SelectItem(IN_STAT_CONC_DEF_LIST, getConstants().instanceStatisticalConceptsDefinitions());
        statConcDefListItem.setMultiple(true);
        classSystemItem = new MultiLanguageTextItem(IN_CLASS_SYSTEM, getConstants().instanceClassSystem());
        classSystemListItem = new SelectItem(IN_CLASS_SYSTEM_LIST, getConstants().instanceClassSystemList());
        classSystemListItem.setMultiple(true);
        contentEditionForm.setFields(dataDescriptionItem, statisticalPopulationItem, statisticalUnitItem, geographicalGranularityItem, geographicalComparabilityItem, temporalGranularityItem,
                basePeriodItem, temporalComparabilityItem, unitMeasureItem, statConcDefItem, statConcDefListItem, classSystemItem, classSystemListItem);

        // Class descriptors
        classEditionForm = new GroupDynamicForm(getConstants().instanceClassDescriptors());
        instanceTypeItem = new SelectItem(IN_INSTANCE_TYPE, getConstants().instanceType());
        classEditionForm.setFields(instanceTypeItem);

        // Production descriptors
        productionEditionForm = new GroupDynamicForm(getConstants().instanceProductionDescriptors());
        ViewTextItem internalInventoryDate = new ViewTextItem(IN_INTERNAL_INVENTORY_DATE, getConstants().instanceInternalInventoryDate());
        ViewTextItem procStatus = new ViewTextItem(IN_PROC_STATUS, getConstants().instanceProcStatus());
        docMethodItem = new MultiLanguageTextAndUrlItem(IN_DOC_METHOD, getConstants().instanceDocMethod());
        surveySourceItem = new SelectItem(IN_SURVEY_SOURCE, getConstants().instanceSurveySource());
        collMethodItem = new SelectItem(IN_COLL_METHOD, getConstants().instanceCollMethod());
        infSuppliersOrganItem = new ExternalMultipleSelectItem(IN_INFORMATION_SUPPLIERS, getConstants().instanceInformationSuppliersOrg());
        infSuppliersOrganItem.getSchemeItem().addChangedHandler(new ChangedHandler() {

            @Override
            public void onChanged(ChangedEvent event) {
                if (event.getValue() != null) {
                    getUiHandlers().populateInfSuppliersOrg(event.getValue().toString());
                }
            }
        });
        infSuppliersConceptsItem = new ExternalMultipleSelectItem(IN_INFORMATION_SUPPLIERS + "-con", getConstants().instanceInformationSuppliersCon());
        infSuppliersConceptsItem.getSchemeItem().addChangedHandler(new ChangedHandler() {

            @Override
            public void onChanged(ChangedEvent event) {
                getUiHandlers().populateInfSuppliersConcept(event.getValue().toString());
            }
        });
        freqCollItem = new SelectItem(IN_FREQ_COLL, getConstants().instanceFreqColl());
        freqCollItem.setMultiple(true);
        dataValidationItem = new MultiLanguageTextAndUrlItem(IN_DATA_VALIDATION, getConstants().instanceDataValidation());
        dataCompilationItem = new MultiLanguageTextAndUrlItem(IN_DATA_COMPILATION, getConstants().instanceDataCompilation());
        adjustmentItem = new MultiLanguageTextAndUrlItem(IN_ADJUSTMENT, getConstants().instanceAdjustment());
        costBurdenItem = new MultiLanguageTextAndUrlItem(IN_COST_BURDEN, getConstants().instanceCostBurden());
        costItem = new SelectItem(IN_COST, getConstants().instanceCost());
        costItem.setMultiple(true);
        productionEditionForm.setFields(internalInventoryDate, procStatus, docMethodItem, surveySourceItem, collMethodItem, infSuppliersOrganItem, infSuppliersConceptsItem, freqCollItem,
                dataValidationItem, dataCompilationItem, adjustmentItem, costBurdenItem, costItem);

        // Diffusion and Publication
        diffusionEditionForm = new GroupDynamicForm(getConstants().instanceDiffusionDescriptors());
        ViewTextItem inventoryDate = new ViewTextItem(IN_INVENTORY_DATE, getConstants().instanceInventoryDate());
        diffusionEditionForm.setFields(inventoryDate);

        // Quality Descriptors
        qualityEditionForm = new GroupDynamicForm(getConstants().instanceQualityDescriptors());
        qualityDocItem = new MultiLanguageTextAndUrlItem(IN_QUALITY_DOC, getConstants().instanceQDoc());
        qualityAssureItem = new MultiLanguageTextAndUrlItem(IN_QUALITY_ASSURE, getConstants().instanceQAssure());
        qualityAssesmentItem = new MultiLanguageTextAndUrlItem(IN_QUALITY_ASSMNT, getConstants().instanceQAssmnt());
        userNeedsItem = new MultiLanguageTextAndUrlItem(IN_USER_NEEDS, getConstants().instanceUserNeeds());
        userSatItem = new MultiLanguageTextAndUrlItem(IN_USER_SAT, getConstants().instanceUserSat());
        completenessItem = new MultiLanguageTextAndUrlItem(IN_COMPLETENESS, getConstants().instanceCompleteness());
        timelinessItem = new MultiLanguageTextAndUrlItem(IN_TIMELINESS, getConstants().instanceTimeliness());
        punctualityItem = new MultiLanguageTextAndUrlItem(IN_PUNCTUALITY, getConstants().instancePunctuality());
        accuracyOverallItem = new MultiLanguageTextAndUrlItem(IN_ACCURACY_OVERALL, getConstants().instanceAccuracyOverall());
        samplingErrItem = new MultiLanguageTextAndUrlItem(IN_SAMPLING_ERROR, getConstants().instanceSamplingErr());
        nonSamplingErrItem = new MultiLanguageTextAndUrlItem(IN_NONSAMPLING_ERR, getConstants().instanceNonSamplingErr());
        coherXDomItem = new MultiLanguageTextAndUrlItem(IN_COHER_X_DOM, getConstants().instanceCoherXDom());
        coherInternalItem = new MultiLanguageTextAndUrlItem(IN_COHER_INTERNAL, getConstants().instanceCoherInter());
        qualityEditionForm.setFields(qualityDocItem, qualityAssureItem, qualityAssesmentItem, userNeedsItem, userSatItem, completenessItem, timelinessItem, punctualityItem, accuracyOverallItem,
                samplingErrItem, nonSamplingErrItem, coherXDomItem, coherInternalItem);

        // Annotations
        annotationsEditionForm = new GroupDynamicForm(getConstants().instanceAnnotations());
        commentItem = new MultiLanguageTextAndUrlItem(IN_COMMENTS, getConstants().instanceComments());
        notesItem = new MultiLanguageTextAndUrlItem(IN_NOTES, getConstants().instanceNotes());
        annotationsEditionForm.setFields(commentItem, notesItem);

        mainFormLayout.addEditionCanvas(identifiersEditionForm);

        mainFormLayout.addEditionCanvas(contentEditionForm);
        mainFormLayout.addEditionCanvas(classEditionForm);
        mainFormLayout.addEditionCanvas(productionEditionForm);
        mainFormLayout.addEditionCanvas(diffusionEditionForm);
        mainFormLayout.addEditionCanvas(qualityEditionForm);
        mainFormLayout.addEditionCanvas(annotationsEditionForm);
    }

    private void setViewForm(InstanceDto instanceDto) {
        // Identifiers
        identifiersViewForm.setValue(IN_IDENTIFIER, instanceDto.getCode());
        identifiersViewForm.setValue(IN_TITLE, RecordUtils.getInternationalStringRecord(instanceDto.getTitle()));
        identifiersViewForm.setValue(IN_ACRONYM, RecordUtils.getInternationalStringRecord(instanceDto.getAcronym()));

        // Content Classifiers

        // Content Descriptors
        contentViewForm.setValue(IN_DATA_DESCRIPTION, RecordUtils.getInternationalStringRecord(instanceDto.getDataDescription()));
        contentViewForm.setValue(IN_STATISTICAL_POPULATION, RecordUtils.getInternationalStringRecord(instanceDto.getStatisticalPopulation()));
        contentViewForm.setValue(IN_STATISTIAL_UNIT, ExternalItemUtils.getExternalItemListToString(instanceDto.getStatisticalUnit()));
        contentViewForm.setValue(IN_GEOGRAPHIC_GRANULARITY, instanceDto.getGeographicGranularity() != null ? instanceDto.getGeographicGranularity().getCodeId() : "");
        contentViewForm.setValue(IN_GEOGRAPHIC_COMPARABILITY, RecordUtils.getInternationalStringRecord(instanceDto.getGeographicComparability()));
        contentViewForm.setValue(IN_TEMPORAL_GRANULARITY, instanceDto.getTemporalGranularity() != null ? instanceDto.getTemporalGranularity().getCodeId() : "");
        contentViewForm.setValue(IN_TEMPORAL_COMPARABILITY, RecordUtils.getInternationalStringRecord(instanceDto.getTemporalComparability()));
        contentViewForm.setValue(IN_BASE_PERIOD, instanceDto.getBasePeriod());
        contentViewForm.setValue(IN_UNIT_MEASURE, ExternalItemUtils.getExternalItemListToString(instanceDto.getUnitMeasure()));
        contentViewForm.setValue(IN_STAT_CONC_DEF, RecordUtils.getInternationalStringRecord(instanceDto.getStatConcDef()));
        contentViewForm.setValue(IN_STAT_CONC_DEF_LIST, ExternalItemUtils.getExternalItemListToString(instanceDto.getStatConcDefList()));
        contentViewForm.setValue(IN_CLASS_SYSTEM, RecordUtils.getInternationalStringRecord(instanceDto.getClassSystem()));
        contentViewForm.setValue(IN_CLASS_SYSTEM_LIST, ExternalItemUtils.getExternalItemListToString(instanceDto.getClassSystemList()));

        // Class descriptors
        classViewForm.setValue(IN_INSTANCE_TYPE, instanceDto.getInstanceType() != null ? instanceDto.getInstanceType().getIdentifier() : "");

        // Production descriptors
        productionViewForm.setValue(IN_INTERNAL_INVENTORY_DATE, instanceDto.getInternalInventoryDate());
        productionViewForm.setValue(IN_PROC_STATUS, getCoreMessages().getString(getCoreMessages().procStatusEnum() + instanceDto.getProcStatus().getName()));

        // Diffusion and Publication
        diffusionViewForm.setValue(IN_INVENTORY_DATE, instanceDto.getInventoryDate());
        staticDocMethodItem.setValue(instanceDto.getDocMethod(), instanceDto.getDocMethodUrl());
        productionViewForm.setValue(IN_SURVEY_SOURCE, instanceDto.getSurveySource() != null ? instanceDto.getSurveySource().getIdentifier() : "");
        productionViewForm.setValue(IN_COLL_METHOD, instanceDto.getCollMethod() != null ? instanceDto.getCollMethod().getIdentifier() : "");
        productionViewForm.setValue(IN_INFORMATION_SUPPLIERS, ExternalItemUtils.getExternalItemListToString(instanceDto.getInformationSuppliers()));
        productionViewForm.setValue(IN_FREQ_COLL, ExternalItemUtils.getExternalItemListToString(instanceDto.getFreqColl()));
        staticDataValidationItem.setValue(instanceDto.getDataValidation(), instanceDto.getDataValidationUrl());
        staticDataCompilationItem.setValue(instanceDto.getDataCompilation(), instanceDto.getDataCompilationUrl());
        staticAdjustmentItem.setValue(instanceDto.getAdjustment(), instanceDto.getAdjustmentUrl());
        staticCostBurdenItem.setValue(instanceDto.getCostBurden(), instanceDto.getCostBurdenUrl());
        productionViewForm.setValue(IN_COST, OperationsListUtils.getCostDtoListToString(instanceDto.getCost()));

        // Quality Descriptors
        staticQualityDocItem.setValue(instanceDto.getQualityDoc(), instanceDto.getQualityDocUrl());
        staticQualityAssureItem.setValue(instanceDto.getQualityAssure(), instanceDto.getQualityAssureUrl());
        staticQualityAssesmentItem.setValue(instanceDto.getQualityAssmnt(), instanceDto.getQualityAssmntUrl());
        staticUserNeedsItem.setValue(instanceDto.getUserNeeds(), instanceDto.getUserNeedsUrl());
        staticUserSatItem.setValue(instanceDto.getUserSat(), instanceDto.getUserSatUrl());
        staticCompletenessItem.setValue(instanceDto.getCompleteness(), instanceDto.getCompletenessUrl());
        staticTimelinessItem.setValue(instanceDto.getTimeliness(), instanceDto.getTimelinessUrl());
        staticPunctualityItem.setValue(instanceDto.getPunctuality(), instanceDto.getPunctualityUrl());
        staticAccuracyOverallItem.setValue(instanceDto.getAccuracyOverall(), instanceDto.getAccuracyOverallUrl());
        staticSamplingErrItem.setValue(instanceDto.getSamplingErr(), instanceDto.getSamplingErrUrl());
        statocNonSamplingErrItem.setValue(instanceDto.getNonsamplingErr(), instanceDto.getNonsamplingErrUrl());
        staticCoherXDomItem.setValue(instanceDto.getCoherXDomain(), instanceDto.getCoherXDomainUrl());
        staticCoherInternalItem.setValue(instanceDto.getCoherInternal(), instanceDto.getCoherInternalUrl());
        qualityViewForm.redraw();
        qualityViewForm.setRedrawOnResize(true);

        // Annotations
        staticCommentItem.setValue(instanceDto.getComment(), instanceDto.getCommentUrl());
        staticNotesItem.setValue(instanceDto.getNotes(), instanceDto.getNotesUrl());
    }

    private void setEditionForm(InstanceDto instanceDto) {
        // Identifiers
        identifier.setValue(instanceDto.getCode());
        title.setValue(instanceDto.getTitle());
        acronym.setValue(instanceDto.getAcronym());

        // Content classifiers

        // Content descriptors
        dataDescriptionItem.setValue(instanceDto.getDataDescription());
        statisticalPopulationItem.setValue(instanceDto.getStatisticalPopulation());
        statisticalUnitItem.clearValue();
        geographicalGranularityItem.setValue(instanceDto.getGeographicGranularity() != null ? instanceDto.getGeographicGranularity().getCodeId() : "");
        geographicalComparabilityItem.setValue(instanceDto.getGeographicComparability());
        temporalGranularityItem.setValue(instanceDto.getTemporalGranularity() != null ? instanceDto.getTemporalGranularity().getCodeId() : "");
        temporalComparabilityItem.setValue(instanceDto.getTemporalComparability());
        contentEditionForm.setValue(IN_BASE_PERIOD, instanceDto.getBasePeriod());
        unitMeasureItem.setValues(ExternalItemUtils.getExternalItemsCodeIds(instanceDto.getUnitMeasure()));
        statConcDefItem.setValue(instanceDto.getStatConcDef());
        statConcDefListItem.setValues(ExternalItemUtils.getExternalItemsCodeIds(instanceDto.getStatConcDefList()));
        classSystemItem.setValue(instanceDto.getClassSystem());
        classSystemListItem.setValues(ExternalItemUtils.getExternalItemsCodeIds(instanceDto.getClassSystemList()));

        // Class descriptors
        instanceTypeItem.setValue(instanceDto.getInstanceType() != null ? instanceDto.getInstanceType().getId().toString() : "");

        // Production descriptors
        diffusionEditionForm.setValue(IN_INVENTORY_DATE, instanceDto.getInventoryDate());
        docMethodItem.setValue(instanceDto.getDocMethod(), instanceDto.getDocMethodUrl());
        surveySourceItem.setValue(instanceDto.getSurveySource() != null ? instanceDto.getSurveySource().getId() : "");
        collMethodItem.setValue(instanceDto.getCollMethod() != null ? instanceDto.getCollMethod().getId() : "");
        infSuppliersConceptsItem.clearValue();
        infSuppliersConceptsItem.clearValue();
        freqCollItem.setValues(ExternalItemUtils.getExternalItemsCodeIds(instanceDto.getFreqColl()));
        dataValidationItem.setValue(instanceDto.getDataValidation(), instanceDto.getDataValidationUrl());
        dataCompilationItem.setValue(instanceDto.getDataCompilation(), instanceDto.getDataCompilationUrl());
        adjustmentItem.setValue(instanceDto.getAdjustment(), instanceDto.getAdjustmentUrl());
        costBurdenItem.setValue(instanceDto.getCostBurden(), instanceDto.getCostBurdenUrl());
        costItem.setValues(getCostIds(instanceDto.getCost()));

        // Diffusion and Publication
        diffusionEditionForm.setValue(IN_INVENTORY_DATE, instanceDto.getInventoryDate());

        // Quality Descriptors
        qualityDocItem.setValue(instanceDto.getQualityDoc(), instanceDto.getQualityDocUrl());
        qualityAssureItem.setValue(instanceDto.getQualityAssure(), instanceDto.getQualityAssureUrl());
        qualityAssesmentItem.setValue(instanceDto.getQualityAssmnt(), instanceDto.getQualityAssmntUrl());
        userNeedsItem.setValue(instanceDto.getUserNeeds(), instanceDto.getUserNeedsUrl());
        userSatItem.setValue(instanceDto.getUserSat(), instanceDto.getUserSatUrl());
        completenessItem.setValue(instanceDto.getCompleteness(), instanceDto.getCompletenessUrl());
        timelinessItem.setValue(instanceDto.getTimeliness(), instanceDto.getTimelinessUrl());
        punctualityItem.setValue(instanceDto.getPunctuality(), instanceDto.getPunctualityUrl());
        accuracyOverallItem.setValue(instanceDto.getAccuracyOverall(), instanceDto.getAccuracyOverallUrl());
        samplingErrItem.setValue(instanceDto.getSamplingErr(), instanceDto.getSamplingErrUrl());
        nonSamplingErrItem.setValue(instanceDto.getNonsamplingErr(), instanceDto.getNonsamplingErrUrl());
        coherXDomItem.setValue(instanceDto.getCoherXDomain(), instanceDto.getCoherXDomainUrl());
        coherInternalItem.setValue(instanceDto.getCoherInternal(), instanceDto.getCoherInternalUrl());

        // Annotations
        commentItem.setValue(instanceDto.getComment(), instanceDto.getCommentUrl());
        notesItem.setValue(instanceDto.getNotes(), instanceDto.getNotesUrl());
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
    public void setOrganisationScheme(List<ExternalItemBtDto> schemes) {
        infSuppliersOrganItem.setSchemesValueMap(ExternalItemUtils.getExternalItemsHashMap(schemes));
    }

    @Override
    public void setConceptScheme(List<ExternalItemBtDto> schemes) {
        this.conceptSchemes = schemes;
        LinkedHashMap<String, String> map = ExternalItemUtils.getExternalItemsHashMap(schemes);
        statisticalUnitItem.setSchemesValueMap(map);
        infSuppliersConceptsItem.setSchemesValueMap(map);
        statConcDefListItem.setValueMap(map);
    }

    @Override
    public void setInfSuppliersOrg(List<ExternalItemBtDto> organisations) {
        this.infSuppliersOrganisations = organisations;
        infSuppliersOrganItem.setItemsValueMap(ExternalItemUtils.getExternalItemsHashMap(organisations));
    }

    @Override
    public void setInfSuppliersConcept(List<ExternalItemBtDto> concepts) {
        this.infSuppliersConcepts = concepts;
        infSuppliersConceptsItem.setItemsValueMap(ExternalItemUtils.getExternalItemsHashMap(concepts));
    }

    @Override
    public void setStatisticalUnitConcepts(List<ExternalItemBtDto> concepts) {
        this.statisticalUnitConcepts = concepts;
        statisticalUnitItem.setItemsValueMap(ExternalItemUtils.getExternalItemsHashMap(concepts));
    }

    @Override
    public void setCodeLists(List<ExternalItemBtDto> codeLists) {
        this.codeLists = codeLists;
        LinkedHashMap<String, String> map = ExternalItemUtils.getExternalItemsHashMap(codeLists);
        geographicalGranularityItem.setValueMap(map);
        unitMeasureItem.setValueMap(map);
        classSystemListItem.setValueMap(map);
    }

    @Override
    public void setTemporalGranularityCodes(List<ExternalItemBtDto> codes) {
        this.temporalGranularityCodes = codes;
        temporalGranularityItem.setValueMap(ExternalItemUtils.getExternalItemsHashMap(codes));
    }

    @Override
    public void setFreqCollCodes(List<ExternalItemBtDto> codes) {
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

}
