package org.siemac.metamac.statistical.operations.web.client.operation.view;

import static org.siemac.metamac.statistical.operations.web.client.OperationsWeb.getConstants;
import static org.siemac.metamac.statistical.operations.web.client.OperationsWeb.getCoreMessages;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;

import org.siemac.metamac.core.common.dto.ExternalItemDto;
import org.siemac.metamac.statistical.operations.core.dto.FamilyBaseDto;
import org.siemac.metamac.statistical.operations.core.dto.InstanceBaseDto;
import org.siemac.metamac.statistical.operations.core.dto.InstanceDto;
import org.siemac.metamac.statistical.operations.core.dto.OfficialityTypeDto;
import org.siemac.metamac.statistical.operations.core.dto.OperationDto;
import org.siemac.metamac.statistical.operations.core.dto.SurveyTypeDto;
import org.siemac.metamac.statistical.operations.core.enume.domain.ProcStatusEnum;
import org.siemac.metamac.statistical.operations.core.enume.domain.StatusEnum;
import org.siemac.metamac.statistical.operations.web.client.enums.ToolStripButtonEnum;
import org.siemac.metamac.statistical.operations.web.client.model.InstanceRecord;
import org.siemac.metamac.statistical.operations.web.client.model.ds.FamilyDS;
import org.siemac.metamac.statistical.operations.web.client.model.ds.OperationDS;
import org.siemac.metamac.statistical.operations.web.client.operation.presenter.OperationPresenter;
import org.siemac.metamac.statistical.operations.web.client.operation.view.handlers.OperationUiHandlers;
import org.siemac.metamac.statistical.operations.web.client.resources.GlobalResources;
import org.siemac.metamac.statistical.operations.web.client.utils.ClientSecurityUtils;
import org.siemac.metamac.statistical.operations.web.client.utils.CommonUtils;
import org.siemac.metamac.statistical.operations.web.client.utils.OperationsListUtils;
import org.siemac.metamac.statistical.operations.web.client.utils.RecordUtils;
import org.siemac.metamac.statistical.operations.web.client.widgets.AddFamiliesToOperationWindow;
import org.siemac.metamac.statistical.operations.web.client.widgets.InstancesOrderFormLayout;
import org.siemac.metamac.statistical.operations.web.client.widgets.ListGridToolStrip;
import org.siemac.metamac.statistical.operations.web.client.widgets.ModalWindow;
import org.siemac.metamac.statistical.operations.web.client.widgets.NewInstanceForm;
import org.siemac.metamac.statistical.operations.web.client.widgets.OperationMainFormLayout;
import org.siemac.metamac.web.common.client.MetamacWebCommon;
import org.siemac.metamac.web.common.client.utils.CommonWebUtils;
import org.siemac.metamac.web.common.client.utils.ExternalItemUtils;
import org.siemac.metamac.web.common.client.utils.FormItemUtils;
import org.siemac.metamac.web.common.client.utils.InternationalStringUtils;
import org.siemac.metamac.web.common.client.widgets.CustomListGrid;
import org.siemac.metamac.web.common.client.widgets.TitleLabel;
import org.siemac.metamac.web.common.client.widgets.form.GroupDynamicForm;
import org.siemac.metamac.web.common.client.widgets.form.fields.CustomCheckboxItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.CustomSelectItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.CustomTextItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.ExternalMultipleSelectItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.ExternalSelectItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.MultiLanguageTextAreaItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.MultiLanguageTextItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.MultilanguageRichTextEditorItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.RequiredTextItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.ViewMultiLanguageTextItem;
import org.siemac.metamac.web.common.client.widgets.form.fields.ViewTextItem;

import com.google.gwt.user.client.ui.Widget;
import com.gwtplatform.mvp.client.ViewWithUiHandlers;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.types.SortDirection;
import com.smartgwt.client.types.Visibility;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.events.HasClickHandlers;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.FormItemIfFunction;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.HasRecordClickHandlers;
import com.smartgwt.client.widgets.grid.events.SelectionChangedHandler;
import com.smartgwt.client.widgets.grid.events.SelectionEvent;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

public class OperationViewImpl extends ViewWithUiHandlers<OperationUiHandlers> implements OperationPresenter.OperationView {

    public static final int                 FAMILY_LIST_MAX_RESULTS = 17;

    private OperationUiHandlers             uiHandlers;

    private VLayout                         panel;

    private OperationMainFormLayout         mainFormLayout;

    private OperationDto                    operationDto;

    // IDENTIFIERS
    private GroupDynamicForm                identifiersViewForm;
    private GroupDynamicForm                identifiersEditionForm;
    private RequiredTextItem                code;
    private MultiLanguageTextItem           title;
    private MultiLanguageTextItem           acronym;

    // CONTENT CLASSIFIERS
    private GroupDynamicForm                classificationViewForm;
    private GroupDynamicForm                classificationEditionForm;
    private ExternalSelectItem              subjectItem;
    private ExternalMultipleSelectItem      secondarySubjectItem;

    // CONTENT DESCRIPTORS
    private GroupDynamicForm                contentViewForm;
    private GroupDynamicForm                contentEditionForm;
    private MultiLanguageTextItem           objective;
    private MultiLanguageTextAreaItem       description;

    // CLASS DESCRIPTORS
    private GroupDynamicForm                classViewForm;
    private GroupDynamicForm                classEditionForm;
    private CustomSelectItem                surveyType;
    private CustomSelectItem                officialityType;
    private CustomCheckboxItem              indSystem;

    // PRODUCTION DESCRIPTORS
    private GroupDynamicForm                productionViewForm;
    private GroupDynamicForm                productionEditionForm;
    private ExternalMultipleSelectItem      producerItem;
    private ExternalMultipleSelectItem      regionalResponsibleItem;
    private ExternalMultipleSelectItem      regionalContributorItem;
    private CustomCheckboxItem              currentlyActiveItem;
    private CustomSelectItem                statusItem;

    // DIFUSSION AND PUBLICATION
    private GroupDynamicForm                diffusionViewForm;
    private GroupDynamicForm                diffusionEditionForm;
    private ExternalMultipleSelectItem      publisherItem;
    private MultilanguageRichTextEditorItem relPolUsAc;
    private CustomCheckboxItem              releaseCalendar;
    private CustomTextItem                  releaseCalendarAccess;
    private CustomSelectItem                updateFrequencyItem;
    private MultilanguageRichTextEditorItem revPolicyItem;
    private MultilanguageRichTextEditorItem revPracticeItem;
    private CustomSelectItem                commonMetadataItem;

    private ViewMultiLanguageTextItem       staticRelPolUsAc;
    private ViewMultiLanguageTextItem       staticRevPolicyItem;
    private ViewMultiLanguageTextItem       staticRevPracticeItem;

    // ANNOTATIONS
    private GroupDynamicForm                annotationsViewForm;
    private GroupDynamicForm                annotationsEditionForm;
    private MultilanguageRichTextEditorItem commentItem;
    private MultilanguageRichTextEditorItem notesItem;

    private ViewMultiLanguageTextItem       staticCommentItem;
    private ViewMultiLanguageTextItem       staticNotesItem;

    private ListGridToolStrip               instanceListGridToolStrip;
    private CustomListGrid                  instanceListGrid;
    private InstancesOrderFormLayout        instancesOrderFormLayout;
    // Instance modal window
    private ModalWindow                     newInstanceWindow;
    private NewInstanceForm                 newInstanceForm;

    private ToolStrip                       familiesToolStrip;
    private ToolStripButton                 editFamiliesToolStripButton;
    private ListGrid                        familyListGrid;
    // Families modal window
    private AddFamiliesToOperationWindow    addFamiliesToOperationWindow;

    private List<FamilyBaseDto>             familyBaseDtos;

    private List<SurveyTypeDto>             surveyTypeDtos;
    private List<OfficialityTypeDto>        officialityTypeDtos;

    private List<ExternalItemDto>           subjects;
    private List<ExternalItemDto>           secondarySubjects;
    private List<ExternalItemDto>           producers;
    private List<ExternalItemDto>           regionalResponsibles;
    private List<ExternalItemDto>           regionalContributors;
    private List<ExternalItemDto>           publishers;
    private List<ExternalItemDto>           commonMetadataList;
    private List<ExternalItemDto>           updateFrequencyCodes;

    public OperationViewImpl() {
        super();
        panel = new VLayout();

        // OPERATION

        mainFormLayout = new OperationMainFormLayout();
        mainFormLayout.getTranslateToolStripButton().addClickHandler(new ClickHandler() {

            @Override
            public void onClick(ClickEvent event) {
                setTranslationsShowed(mainFormLayout.getTranslateToolStripButton().isSelected());
            }
        });
        createViewForm();
        createEditionForm();

        // INSTANCES

        newInstanceForm = new NewInstanceForm();

        newInstanceWindow = new ModalWindow();
        newInstanceWindow.setTitle(getConstants().actionNewInstance());
        newInstanceWindow.setAutoSize(true);
        newInstanceWindow.addItem(newInstanceForm);

        instanceListGridToolStrip = new ListGridToolStrip(getConstants().instanceDeleteConfirmation());
        instanceListGridToolStrip.getNewButton().addClickHandler(new ClickHandler() {

            @Override
            public void onClick(ClickEvent event) {
                // Clear new instance form
                newInstanceForm.clearValues();

                newInstanceWindow.show();
            }
        });

        TitleLabel instancesTitleLabel = new TitleLabel(getConstants().instances());
        instancesTitleLabel.setStyleName("sectionTitleLeftMargin");

        // Instances list
        instanceListGrid = new CustomListGrid();
        instanceListGrid.setHeight(150);
        ListGridField identifierField = new ListGridField(InstanceRecord.CODE, getCoreMessages().instance_code());
        ListGridField titleField = new ListGridField(InstanceRecord.TITLE, getCoreMessages().instance_title());
        ListGridField descriptionField = new ListGridField(InstanceRecord.DESCRIPTION, getCoreMessages().instance_description());
        ListGridField statusField = new ListGridField(InstanceRecord.STATUS, getConstants().instanceStatus());
        ListGridField orderField = new ListGridField(InstanceRecord.ORDER, getCoreMessages().instance_order());
        instanceListGrid.setFields(identifierField, titleField, descriptionField, statusField, orderField);
        instanceListGrid.addSelectionChangedHandler(new SelectionChangedHandler() {

            @Override
            public void onSelectionChanged(SelectionEvent event) {
                if (instanceListGrid.getSelectedRecords() != null && instanceListGrid.getSelectedRecords().length == 1) {
                    InstanceRecord record = (InstanceRecord) instanceListGrid.getSelectedRecord();
                    selectInstance(record.getId());
                } else {
                    // No record selected
                    deselectInstance();
                    if (instanceListGrid.getSelectedRecords().length > 1) {
                        // Delete more than one Instance with one click
                        showInstanceListGridDeleteButton();
                    }
                }
            }
        });

        VLayout instancesListGridLayout = new VLayout();
        instancesListGridLayout.setMargin(15);
        instancesListGridLayout.addMember(instanceListGridToolStrip);
        instancesListGridLayout.addMember(instanceListGrid);

        // Instances order
        instancesOrderFormLayout = new InstancesOrderFormLayout();
        instancesOrderFormLayout.getSave().addClickHandler(new ClickHandler() {

            @Override
            public void onClick(ClickEvent event) {
                uiHandlers.updateInstancesOrder(instancesOrderFormLayout.getInstancesOrder());
            }
        });

        // FAMILIES

        familiesToolStrip = new ToolStrip();
        familiesToolStrip.setWidth100();
        editFamiliesToolStripButton = new ToolStripButton(getConstants().actionEdit(), GlobalResources.RESOURCE.editListGrid().getURL());
        editFamiliesToolStripButton.addClickHandler(new ClickHandler() {

            @Override
            public void onClick(ClickEvent event) {
                // Load operation families
                uiHandlers.retrievePaginatedFamilies(0, FAMILY_LIST_MAX_RESULTS, null);

                addFamiliesToOperationWindow = new AddFamiliesToOperationWindow(uiHandlers);
                addFamiliesToOperationWindow.setSelectedFamilies(familyBaseDtos);
            }
        });
        familiesToolStrip.addButton(editFamiliesToolStripButton);

        TitleLabel familiesTitleLabel = new TitleLabel(getConstants().families());
        familiesTitleLabel.setStyleName("sectionTitleLeftMargin");

        familyListGrid = new ListGrid();
        familyListGrid.setHeight(150);
        ListGridField identifierFamilyField = new ListGridField(FamilyDS.CODE, getCoreMessages().family_code());
        ListGridField titleFamilyField = new ListGridField(FamilyDS.TITLE, getCoreMessages().family_title());
        ListGridField descriptionFamilyField = new ListGridField(FamilyDS.DESCRIPTION, getCoreMessages().family_description());
        ListGridField statusFamilyField = new ListGridField(FamilyDS.PROC_STATUS, getCoreMessages().family_proc_status());
        familyListGrid.setFields(identifierFamilyField, titleFamilyField, descriptionFamilyField, statusFamilyField);

        VLayout familiesListGridLayout = new VLayout();
        familiesListGridLayout.setMargin(15);
        familiesListGridLayout.addMember(familiesToolStrip);
        familiesListGridLayout.addMember(familyListGrid);

        VLayout subPanel = new VLayout();
        subPanel.setHeight100();
        subPanel.setOverflow(Overflow.SCROLL);
        subPanel.addMember(mainFormLayout);

        subPanel.addMember(instancesTitleLabel);
        subPanel.addMember(instancesListGridLayout);
        subPanel.addMember(instancesOrderFormLayout);

        subPanel.addMember(familiesTitleLabel);
        subPanel.addMember(familiesListGridLayout);

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
        if (slot == OperationPresenter.TYPE_SetContextAreaContentToolBar) {
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
    public HasRecordClickHandlers getSelectedFamily() {
        return familyListGrid;
    }

    @Override
    public HasRecordClickHandlers getSelectedInstance() {
        return instanceListGrid;
    }

    @Override
    public void setOperation(OperationDto operationDto, List<InstanceBaseDto> instanceBaseDtos, List<FamilyBaseDto> familyBaseDtos) {
        this.operationDto = operationDto;

        // Security
        mainFormLayout.setCanEdit(ClientSecurityUtils.canUpdateOperation(operationDto.getCode()));
        mainFormLayout.setOperationCode(operationDto.getCode());
        instancesOrderFormLayout.setCanEdit(ClientSecurityUtils.canUpdateInstancesOrder(operationDto.getCode()));
        instanceListGridToolStrip.getNewButton().setVisibility(ClientSecurityUtils.canCreateInstance(operationDto.getCode()) ? Visibility.VISIBLE : Visibility.HIDDEN);
        editFamiliesToolStripButton.setVisibility(ClientSecurityUtils.canAddFamilyToOperation(operationDto.getCode()) ? Visibility.VISIBLE : Visibility.HIDDEN);

        // Load common metadata configurations
        uiHandlers.retrieveCommonMetadataConfigurations();

        // Operation
        setOperation(operationDto);

        // Set Instances
        setInstances(instanceBaseDtos);

        // Set Families
        setOperationFamilies(familyBaseDtos);
    }

    @Override
    public OperationDto getOperation(OperationDto operationDto) {
        // Identifiers
        operationDto.setCode(code.getValueAsString());
        operationDto.setTitle(title.getValue());
        operationDto.setAcronym(acronym.getValue());
        // Content classifiers
        operationDto.setSubjectArea(subjectItem.getSelectedExternalItem(subjects));
        operationDto.getSecondarySubjectAreas().clear();
        operationDto.getSecondarySubjectAreas().addAll(secondarySubjectItem.getSelectedExternalItems(secondarySubjects));
        // Content Descriptors
        operationDto.setDescription(description.getValue());
        operationDto.setObjective(objective.getValue());

        // Class Descriptors
        operationDto.setSurveyType(OperationsListUtils.getSurveyTypeDto(surveyType.getValueAsString(), surveyTypeDtos));
        operationDto.setOfficialityType(OperationsListUtils.getOfficialityTypeDto(officialityType.getValueAsString(), officialityTypeDtos));
        operationDto.setIndicatorSystem(indSystem.getValueAsBoolean() == null ? false : indSystem.getValueAsBoolean());

        // Production descriptors
        operationDto.getProducer().clear();
        operationDto.getProducer().addAll(producerItem.getSelectedExternalItems(producers));
        operationDto.getRegionalResponsible().clear();
        operationDto.getRegionalResponsible().addAll(regionalResponsibleItem.getSelectedExternalItems(regionalResponsibles));
        operationDto.getRegionalContributor().clear();
        operationDto.getRegionalContributor().addAll(regionalContributorItem.getSelectedExternalItems(regionalContributors));
        operationDto.setCurrentlyActive(currentlyActiveItem.getValueAsBoolean());
        operationDto.setStatus(statusItem.getValueAsString() != null ? StatusEnum.valueOf(statusItem.getValueAsString()) : null);

        // Diffusion and Publication
        operationDto.getPublisher().clear();
        operationDto.getPublisher().addAll(publisherItem.getSelectedExternalItems(publishers));
        operationDto.setRelPolUsAc(relPolUsAc.getValue());
        operationDto.setReleaseCalendar(releaseCalendar.getValueAsBoolean());
        operationDto.setReleaseCalendarAccess(releaseCalendarAccess.getValueAsString());
        operationDto.getUpdateFrequency().clear();
        operationDto.getUpdateFrequency().addAll(ExternalItemUtils.getExternalItemDtoListFromUrns(updateFrequencyCodes, updateFrequencyItem.getValues()));
        operationDto.setRevPolicy(revPolicyItem.getValue());
        operationDto.setRevPractice(revPracticeItem.getValue());
        operationDto.setCommonMetadata(ExternalItemUtils.getExternalItemDtoFromUrn(commonMetadataList, commonMetadataItem.getValueAsString()));

        // ANNOTATIONS
        operationDto.setComment(commentItem.getValue());
        operationDto.setNotes(notesItem.getValue());
        return operationDto;
    }

    @Override
    public HasClickHandlers getSave() {
        return mainFormLayout.getSave();
    }

    @Override
    public void onOperationSaved(OperationDto operationDto) {
        setOperation(operationDto);
    }

    @Override
    public boolean validate() {
        return identifiersEditionForm.validate(false) && productionEditionForm.validate(false) && contentEditionForm.validate(false) && classificationEditionForm.validate(false)
                && diffusionEditionForm.validate(false) && subjectItem.validateItem();
    }

    @Override
    public com.smartgwt.client.widgets.form.fields.events.HasClickHandlers getSaveNewInstance() {
        return newInstanceForm.getSave();
    }

    @Override
    public InstanceDto getNewInstace() {
        return newInstanceForm.getInstance();
    }

    @Override
    public void onInstanceSaved(InstanceDto instanceDto) {
        InstanceRecord record = RecordUtils.getInstanceRecord(instanceDto);
        instanceListGrid.addData(record);
        instanceListGrid.sort(InstanceRecord.ORDER, SortDirection.DESCENDING);

        instancesOrderFormLayout.addInstance(instanceDto);
    }

    @Override
    public boolean validateNewInstance() {
        return newInstanceForm.validate();
    }

    @Override
    public void closeInstanceWindow() {
        newInstanceWindow.hide();
    }

    @Override
    public HasClickHandlers getDeleteInstance() {
        return instanceListGridToolStrip.getDeleteConfirmationWindow().getYesButton();
    }

    @Override
    public List<Long> getSelectedInstances() {
        List<Long> selectedInstances = new ArrayList<Long>();
        if (instanceListGrid.getSelectedRecords() != null) {
            ListGridRecord[] records = instanceListGrid.getSelectedRecords();
            for (int i = 0; i < records.length; i++) {
                InstanceRecord record = (InstanceRecord) records[i];
                selectedInstances.add(record.getId());
            }
        }
        return selectedInstances;
    }

    @Override
    public void setInstances(List<InstanceBaseDto> instanceBaseDtos) {
        // Instances list
        instanceListGrid.removeAllData();
        if (instanceBaseDtos != null) {
            for (InstanceBaseDto instanceBaseDto : instanceBaseDtos) {
                instanceListGrid.addData(RecordUtils.getInstanceRecord(instanceBaseDto));
            }
        }
        deselectInstance();

        // Instances order
        instancesOrderFormLayout.setInstances(instanceBaseDtos);
        if (instanceBaseDtos == null || instanceBaseDtos.isEmpty()) {
            instancesOrderFormLayout.hide();
        } else {
            instancesOrderFormLayout.show();
        }
    }

    public void setOperationFamilies(List<FamilyBaseDto> familyBaseDtos) {
        this.familyBaseDtos = familyBaseDtos;
        // Set families in listGrid
        familyListGrid.selectAllRecords();
        familyListGrid.removeSelectedData();
        familyListGrid.deselectAllRecords();
        if (familyBaseDtos != null) {
            for (FamilyBaseDto familyBaseDto : familyBaseDtos) {
                familyListGrid.addData(RecordUtils.getFamilyRecord(familyBaseDto));
            }
        }
    }

    private void setOperation(OperationDto operationDto) {
        mainFormLayout.setViewMode();
        mainFormLayout.updatePublishSection(operationDto.getProcStatus());
        // Set Family
        mainFormLayout.setTitleLabelContents(InternationalStringUtils.getLocalisedString(operationDto.getTitle()));
        // Form
        setViewForm(operationDto);
        setEditionForm(operationDto);
    }

    private void createViewForm() {
        // Identifiers
        identifiersViewForm = new GroupDynamicForm(getConstants().operationIdentifiers());
        ViewTextItem identifier = new ViewTextItem(OperationDS.OP_CODE, getCoreMessages().operation_code());
        ViewMultiLanguageTextItem title = new ViewMultiLanguageTextItem(OperationDS.OP_TITLE, getCoreMessages().operation_title());
        ViewMultiLanguageTextItem acronym = new ViewMultiLanguageTextItem(OperationDS.OP_ACRONYM, getCoreMessages().operation_acronym());
        identifiersViewForm.setFields(identifier, title, acronym);

        // Content Classifiers
        classificationViewForm = new GroupDynamicForm(getConstants().operationContentClassifiers());
        ViewTextItem subject = new ViewTextItem(OperationDS.OP_SUBJECT_AREA, getCoreMessages().operation_subject_area());
        ViewTextItem secondarySubject = new ViewTextItem(OperationDS.OP_SUBJECT_SECONDARY, getCoreMessages().operation_secondary_subject_areas());
        classificationViewForm.setFields(subject, secondarySubject);

        // Content Descriptors
        contentViewForm = new GroupDynamicForm(getConstants().operationContentDescriptors());
        ViewMultiLanguageTextItem description = new ViewMultiLanguageTextItem(OperationDS.OP_DESCRIPTION, getCoreMessages().operation_description());
        ViewMultiLanguageTextItem objective = new ViewMultiLanguageTextItem(OperationDS.OP_OBJECTIVE, getCoreMessages().operation_objective());
        contentViewForm.setFields(objective, description);

        // Class Descriptors
        classViewForm = new GroupDynamicForm(getConstants().operationClassDescriptors());
        ViewTextItem survey = new ViewTextItem(OperationDS.OP_SURVEY_TYPE, getCoreMessages().operation_survey_type());
        ViewTextItem officiality = new ViewTextItem(OperationDS.OP_OFFICIALITY_TYPE, getCoreMessages().operation_officiality_type());
        ViewTextItem indSystem = new ViewTextItem(OperationDS.OP_INDICATOR_SYSTEM, getCoreMessages().operation_indicator_system());
        classViewForm.setFields(survey, officiality, indSystem);

        // Production descriptors
        productionViewForm = new GroupDynamicForm(getConstants().operationProductionDescriptors());
        ViewTextItem producer = new ViewTextItem(OperationDS.OP_PRODUCER, getCoreMessages().operation_producer());
        ViewTextItem regionalResposible = new ViewTextItem(OperationDS.OP_REG_RESPONSIBLE, getCoreMessages().operation_regional_responsible());
        ViewTextItem regionalCont = new ViewTextItem(OperationDS.OP_REG_CONTRIBUTOR, getCoreMessages().operation_regional_contributor());
        ViewTextItem inventoryDate = new ViewTextItem(OperationDS.OP_INTERNAL_INVENTORY_DATE, getCoreMessages().operation_internal_inventory_date());
        ViewTextItem currentlyActive = new ViewTextItem(OperationDS.OP_CURRENTLY_ACTIVE, getCoreMessages().operation_currently_active());
        ViewTextItem status = new ViewTextItem(OperationDS.OP_STATUS, getCoreMessages().operation_status());
        ViewTextItem procStatus = new ViewTextItem(OperationDS.OP_PROC_STATUS, getCoreMessages().operation_proc_status());
        productionViewForm.setFields(producer, regionalResposible, regionalCont, inventoryDate, currentlyActive, status, procStatus);

        // Diffusion Descriptors
        diffusionViewForm = new GroupDynamicForm(getConstants().operationDiffusionAndPublication());
        ViewTextItem publisher = new ViewTextItem(OperationDS.OP_PUBLISHER, getCoreMessages().operation_publisher());
        staticRelPolUsAc = new ViewMultiLanguageTextItem(OperationDS.OP_RE_POL_US_AC, getCoreMessages().operation_rel_pol_us_ac());
        ViewTextItem releaseCalendar = new ViewTextItem(OperationDS.OP_RELEASE_CALENDAR, getConstants().operationReleaseCalendar());
        ViewTextItem releaseCalendarAccess = new ViewTextItem(OperationDS.OP_RELEASE_CALENDAR_ACCESS, getCoreMessages().operation_release_calendar_access());
        ViewTextItem updateFreq = new ViewTextItem(OperationDS.OP_UPDATE_FREQ, getCoreMessages().operation_update_frequency());
        ViewTextItem currentInst = new ViewTextItem(OperationDS.OP_CURRENT_INSTANCE, getConstants().operationCurrentInstance());
        ViewTextItem currentInternalInst = new ViewTextItem(OperationDS.OP_CURRENT_INTERNAL_INSTANCE, getConstants().operationCurrentInternalInstance());
        ViewTextItem invDate = new ViewTextItem(OperationDS.OP_INVENTORY_DATE, getCoreMessages().operation_inventory_date());
        staticRevPolicyItem = new ViewMultiLanguageTextItem(OperationDS.OP_REV_POLICY, getCoreMessages().operation_rev_policy());
        staticRevPracticeItem = new ViewMultiLanguageTextItem(OperationDS.OP_REV_PRACTICE, getCoreMessages().operation_rev_practice());
        ViewTextItem commonMetadata = new ViewTextItem(OperationDS.OP_COMMON_METADATA, getCoreMessages().operation_common_metadata());
        diffusionViewForm.setFields(publisher, staticRelPolUsAc, releaseCalendar, releaseCalendarAccess, updateFreq, currentInst, currentInternalInst, invDate, staticRevPolicyItem,
                staticRevPracticeItem, commonMetadata);

        // Annotations
        annotationsViewForm = new GroupDynamicForm(getConstants().operationAnnotations());
        staticCommentItem = new ViewMultiLanguageTextItem(OperationDS.OP_COMMENTS, getCoreMessages().operation_comment());
        staticNotesItem = new ViewMultiLanguageTextItem(OperationDS.OP_NOTES, getCoreMessages().operation_notes());
        annotationsViewForm.setFields(staticCommentItem, staticNotesItem);

        // Add to main layout
        mainFormLayout.addViewCanvas(identifiersViewForm);
        mainFormLayout.addViewCanvas(classificationViewForm);
        mainFormLayout.addViewCanvas(contentViewForm);
        mainFormLayout.addViewCanvas(classViewForm);
        mainFormLayout.addViewCanvas(productionViewForm);
        mainFormLayout.addViewCanvas(diffusionViewForm);
        mainFormLayout.addViewCanvas(annotationsViewForm);
    }

    private void createEditionForm() {
        // Identifiers
        identifiersEditionForm = new GroupDynamicForm(getConstants().operationIdentifiers());

        code = new RequiredTextItem(OperationDS.OP_CODE, getCoreMessages().operation_code());
        code.setShowIfCondition(new FormItemIfFunction() {

            @Override
            public boolean execute(FormItem item, Object value, DynamicForm form) {
                return canOperationCodeBeEdited();
            }
        });
        code.setValidators(CommonWebUtils.getSemanticIdentifierCustomValidator());
        ViewTextItem staticCode = new ViewTextItem(OperationDS.OP_CODE_VIEW, getCoreMessages().operation_code());
        staticCode.setShowIfCondition(new FormItemIfFunction() {

            @Override
            public boolean execute(FormItem item, Object value, DynamicForm form) {
                return !canOperationCodeBeEdited();
            }
        });

        title = new MultiLanguageTextItem(OperationDS.OP_TITLE, getCoreMessages().operation_title());
        title.setRequired(true);
        acronym = new MultiLanguageTextItem(OperationDS.OP_ACRONYM, getCoreMessages().operation_acronym());
        identifiersEditionForm.setFields(staticCode, code, title, acronym);

        // Content classifiers
        classificationEditionForm = new GroupDynamicForm(getConstants().operationContentClassifiers());
        subjectItem = new ExternalSelectItem(OperationDS.OP_SUBJECT_AREA, getCoreMessages().operation_subject_area());
        subjectItem.setRequired(true);
        subjectItem.getSchemeItem().addChangedHandler(new ChangedHandler() {

            @Override
            public void onChanged(ChangedEvent event) {
                if (event.getValue() != null) {
                    uiHandlers.populateSubjects(event.getValue().toString());
                }
            }
        });
        secondarySubjectItem = new ExternalMultipleSelectItem(OperationDS.OP_SUBJECT_SECONDARY, getCoreMessages().operation_secondary_subject_areas());
        secondarySubjectItem.getSchemeItem().addChangedHandler(new ChangedHandler() {

            @Override
            public void onChanged(ChangedEvent event) {
                if (event.getValue() != null) {
                    uiHandlers.populateSecondarySubjects(event.getValue().toString());
                }
            }
        });
        classificationEditionForm.setFields(subjectItem, secondarySubjectItem);

        // Content Descriptors
        contentEditionForm = new GroupDynamicForm(getConstants().operationContentDescriptors());
        description = new MultiLanguageTextAreaItem(OperationDS.OP_DESCRIPTION, getCoreMessages().operation_description());
        objective = new MultiLanguageTextItem(OperationDS.OP_OBJECTIVE, getCoreMessages().operation_objective());
        // objective.setValidators(getRequiredIfInternallyPublished());
        contentEditionForm.setFields(objective, description);

        // Class Descriptors
        classEditionForm = new GroupDynamicForm(getConstants().operationClassDescriptors());
        surveyType = new CustomSelectItem(OperationDS.OP_SURVEY_TYPE, getCoreMessages().operation_survey_type());
        // surveyType.setValidators(getRequiredIfInternallyPublished());
        officialityType = new CustomSelectItem(OperationDS.OP_OFFICIALITY_TYPE, getCoreMessages().operation_officiality_type());
        // officialityType.setValidators(getRequiredIfInternallyPublished());
        indSystem = new CustomCheckboxItem(OperationDS.OP_INDICATOR_SYSTEM, getCoreMessages().operation_indicator_system());
        indSystem.setTitleStyle("requiredFormLabel");
        classEditionForm.setFields(surveyType, officialityType, indSystem);

        // Production descriptors
        productionEditionForm = new GroupDynamicForm(getConstants().operationProductionDescriptors());
        producerItem = new ExternalMultipleSelectItem(OperationDS.OP_PRODUCER, getCoreMessages().operation_producer());
        // producerItem.setValidators(getRequiredIfInternallyPublished());
        producerItem.getSchemeItem().addChangedHandler(new ChangedHandler() {

            @Override
            public void onChanged(ChangedEvent event) {
                if (event.getValue() != null) {
                    uiHandlers.populateProducers(event.getValue().toString());
                }
            }
        });
        regionalResponsibleItem = new ExternalMultipleSelectItem(OperationDS.OP_REG_RESPONSIBLE, getCoreMessages().operation_regional_responsible());
        // regionalResponsibleItem.setValidators(getRequiredIfInternallyPublished());
        regionalResponsibleItem.getSchemeItem().addChangedHandler(new ChangedHandler() {

            @Override
            public void onChanged(ChangedEvent event) {
                if (event.getValue() != null) {
                    uiHandlers.populateRegionalResposibles(event.getValue().toString());
                }
            }
        });
        regionalContributorItem = new ExternalMultipleSelectItem(OperationDS.OP_REG_CONTRIBUTOR, getCoreMessages().operation_regional_contributor());
        regionalContributorItem.getSchemeItem().addChangedHandler(new ChangedHandler() {

            @Override
            public void onChanged(ChangedEvent event) {
                if (event.getValue() != null) {
                    uiHandlers.populateRegionalContributors(event.getValue().toString());
                }
            }
        });
        ViewTextItem inventoryDate = new ViewTextItem(OperationDS.OP_INTERNAL_INVENTORY_DATE, getCoreMessages().operation_internal_inventory_date());
        currentlyActiveItem = new CustomCheckboxItem(OperationDS.OP_CURRENTLY_ACTIVE, getCoreMessages().operation_currently_active());
        // currentlyActiveItem.setValidators(getRequiredIfInternallyPublished());
        statusItem = new CustomSelectItem(OperationDS.OP_STATUS, getCoreMessages().operation_status());
        statusItem.setValueMap(CommonUtils.getStatusEnumHashMap());
        ViewTextItem procStatus = new ViewTextItem(OperationDS.OP_PROC_STATUS, getCoreMessages().operation_proc_status());
        ViewTextItem staticProcStatus = new ViewTextItem(OperationDS.OP_PROC_STATUS_VIEW, getCoreMessages().operation_proc_status());
        staticProcStatus.setShowIfCondition(FormItemUtils.getFalseFormItemIfFunction());
        productionEditionForm.setFields(producerItem, regionalResponsibleItem, regionalContributorItem, inventoryDate, currentlyActiveItem, statusItem, staticProcStatus, procStatus);

        // Diffusion and Publication
        diffusionEditionForm = new GroupDynamicForm(getConstants().operationDiffusionAndPublication());
        publisherItem = new ExternalMultipleSelectItem(OperationDS.OP_PUBLISHER, getCoreMessages().operation_publisher());
        // publisherItem.setValidators(getRequiredIfInternallyPublished());
        publisherItem.getSchemeItem().addChangedHandler(new ChangedHandler() {

            @Override
            public void onChanged(ChangedEvent event) {
                if (event.getValue() != null) {
                    uiHandlers.populatePublishers(event.getValue().toString());
                }
            }
        });
        relPolUsAc = new MultilanguageRichTextEditorItem(OperationDS.OP_RE_POL_US_AC, getCoreMessages().operation_rel_pol_us_ac());
        releaseCalendar = new CustomCheckboxItem(OperationDS.OP_RELEASE_CALENDAR, getConstants().operationReleaseCalendar());
        releaseCalendarAccess = new CustomTextItem(OperationDS.OP_RELEASE_CALENDAR_ACCESS, getCoreMessages().operation_release_calendar_access());
        releaseCalendarAccess.setValidators(CommonWebUtils.getUrlValidator());
        updateFrequencyItem = new CustomSelectItem(OperationDS.OP_UPDATE_FREQ, getCoreMessages().operation_update_frequency());
        updateFrequencyItem.setMultiple(true);
        ViewTextItem currentInst = new ViewTextItem(OperationDS.OP_CURRENT_INSTANCE, getConstants().operationCurrentInstance());
        ViewTextItem currentInternalInst = new ViewTextItem(OperationDS.OP_CURRENT_INTERNAL_INSTANCE, getConstants().operationCurrentInternalInstance());
        ViewTextItem invDate = new ViewTextItem(OperationDS.OP_INVENTORY_DATE, getCoreMessages().operation_inventory_date());
        revPolicyItem = new MultilanguageRichTextEditorItem(OperationDS.OP_REV_POLICY, getCoreMessages().operation_rev_policy());
        revPracticeItem = new MultilanguageRichTextEditorItem(OperationDS.OP_REV_PRACTICE, getConstants().operationRevPractice());
        commonMetadataItem = new CustomSelectItem(OperationDS.OP_COMMON_METADATA, getCoreMessages().operation_common_metadata());
        // commonMetadataItem.setValidators(getRequiredIfInternallyPublished());
        diffusionEditionForm.setFields(publisherItem, relPolUsAc, releaseCalendar, releaseCalendarAccess, updateFrequencyItem, currentInst, currentInternalInst, invDate, revPolicyItem,
                revPracticeItem, commonMetadataItem);

        // Annotations
        annotationsEditionForm = new GroupDynamicForm(getConstants().operationAnnotations());
        commentItem = new MultilanguageRichTextEditorItem(OperationDS.OP_COMMENTS, getCoreMessages().operation_comment());
        notesItem = new MultilanguageRichTextEditorItem(OperationDS.OP_NOTES, getCoreMessages().operation_notes());
        annotationsEditionForm.setFields(commentItem, notesItem);

        // Add to main layout
        mainFormLayout.addEditionCanvas(identifiersEditionForm);
        mainFormLayout.addEditionCanvas(classificationEditionForm);
        mainFormLayout.addEditionCanvas(contentEditionForm);
        mainFormLayout.addEditionCanvas(classEditionForm);
        mainFormLayout.addEditionCanvas(productionEditionForm);
        mainFormLayout.addEditionCanvas(diffusionEditionForm);
        mainFormLayout.addEditionCanvas(annotationsEditionForm);
    }

    private void setViewForm(OperationDto operationDto) {
        // Identifiers
        identifiersViewForm.setValue(OperationDS.OP_CODE, operationDto.getCode());
        identifiersViewForm.setValue(OperationDS.OP_TITLE, org.siemac.metamac.web.common.client.utils.RecordUtils.getInternationalStringRecord(operationDto.getTitle()));
        identifiersViewForm.setValue(OperationDS.OP_ACRONYM, org.siemac.metamac.web.common.client.utils.RecordUtils.getInternationalStringRecord(operationDto.getAcronym()));

        // Content Classifiers
        classificationViewForm.setValue(OperationDS.OP_SUBJECT_AREA, operationDto.getSubjectArea() == null ? "" : ExternalItemUtils.getExternalItemName(operationDto.getSubjectArea()));
        classificationViewForm.setValue(OperationDS.OP_SUBJECT_SECONDARY, ExternalItemUtils.getExternalItemListToString(operationDto.getSecondarySubjectAreas()));

        // Content Descriptors
        contentViewForm.setValue(OperationDS.OP_DESCRIPTION, org.siemac.metamac.web.common.client.utils.RecordUtils.getInternationalStringRecord(operationDto.getDescription()));
        contentViewForm.setValue(OperationDS.OP_OBJECTIVE, org.siemac.metamac.web.common.client.utils.RecordUtils.getInternationalStringRecord(operationDto.getObjective()));

        // Class Descriptors
        classViewForm.setValue(OperationDS.OP_SURVEY_TYPE,
                operationDto.getSurveyType() == null ? "" : CommonWebUtils.getElementName(operationDto.getSurveyType().getIdentifier(), operationDto.getSurveyType().getDescription()));
        classViewForm.setValue(OperationDS.OP_OFFICIALITY_TYPE,
                operationDto.getOfficialityType() == null ? "" : CommonWebUtils.getElementName(operationDto.getOfficialityType().getIdentifier(), operationDto.getOfficialityType().getDescription()));
        classViewForm.setValue(OperationDS.OP_INDICATOR_SYSTEM, (operationDto.getIndicatorSystem() != null && operationDto.getIndicatorSystem())
                ? MetamacWebCommon.getConstants().yes()
                : MetamacWebCommon.getConstants().no());

        // Production Descriptors
        productionViewForm.setValue(OperationDS.OP_PRODUCER, ExternalItemUtils.getExternalItemListToString(operationDto.getProducer()));
        productionViewForm.setValue(OperationDS.OP_REG_RESPONSIBLE, ExternalItemUtils.getExternalItemListToString(operationDto.getRegionalResponsible()));
        productionViewForm.setValue(OperationDS.OP_REG_CONTRIBUTOR, ExternalItemUtils.getExternalItemListToString(operationDto.getRegionalContributor()));
        productionViewForm.setValue(OperationDS.OP_INTERNAL_INVENTORY_DATE, operationDto.getInternalInventoryDate());
        productionViewForm.setValue(OperationDS.OP_CURRENTLY_ACTIVE, (operationDto.getCurrentlyActive() != null && operationDto.getCurrentlyActive())
                ? MetamacWebCommon.getConstants().yes()
                : MetamacWebCommon.getConstants().no());
        productionViewForm.setValue(OperationDS.OP_STATUS, operationDto.getStatus() == null ? null : getCoreMessages().getString(getCoreMessages().statusEnum() + operationDto.getStatus().getName()));
        productionViewForm.setValue(OperationDS.OP_PROC_STATUS, getCoreMessages().getString(getCoreMessages().procStatusEnum() + operationDto.getProcStatus().getName()));

        // Diffusion and Publication
        diffusionViewForm.setValue(OperationDS.OP_PUBLISHER, ExternalItemUtils.getExternalItemListToString(operationDto.getPublisher()));
        diffusionViewForm.setValue(OperationDS.OP_RE_POL_US_AC, org.siemac.metamac.web.common.client.utils.RecordUtils.getInternationalStringRecord(operationDto.getRelPolUsAc()));
        diffusionViewForm.setValue(OperationDS.OP_RELEASE_CALENDAR, (operationDto.getReleaseCalendar() != null && operationDto.getReleaseCalendar())
                ? MetamacWebCommon.getConstants().yes()
                : MetamacWebCommon.getConstants().no());
        diffusionViewForm.setValue(OperationDS.OP_RELEASE_CALENDAR_ACCESS, operationDto.getReleaseCalendarAccess());
        diffusionViewForm.setValue(OperationDS.OP_UPDATE_FREQ, ExternalItemUtils.getExternalItemListToString(operationDto.getUpdateFrequency()));
        diffusionViewForm.setValue(OperationDS.OP_CURRENT_INSTANCE,
                operationDto.getCurrentInstance() != null ? CommonWebUtils.getElementName(operationDto.getCurrentInstance().getCode(), operationDto.getCurrentInstance().getTitle()) : "");
        diffusionViewForm.setValue(OperationDS.OP_INVENTORY_DATE, operationDto.getInventoryDate());
        diffusionViewForm.setValue(OperationDS.OP_REV_POLICY, org.siemac.metamac.web.common.client.utils.RecordUtils.getInternationalStringRecord(operationDto.getRevPolicy()));
        diffusionViewForm.setValue(OperationDS.OP_REV_PRACTICE, org.siemac.metamac.web.common.client.utils.RecordUtils.getInternationalStringRecord(operationDto.getRevPractice()));

        diffusionViewForm.setValue(OperationDS.OP_COMMON_METADATA, operationDto.getCommonMetadata() != null ? ExternalItemUtils.getExternalItemName(operationDto.getCommonMetadata()) : "");

        // Annotations
        annotationsViewForm.setValue(OperationDS.OP_COMMENTS, org.siemac.metamac.web.common.client.utils.RecordUtils.getInternationalStringRecord(operationDto.getComment()));
        annotationsViewForm.setValue(OperationDS.OP_NOTES, org.siemac.metamac.web.common.client.utils.RecordUtils.getInternationalStringRecord(operationDto.getNotes()));
    }
    private void setEditionForm(OperationDto operationDto) {
        // Identifiers
        code.setValue(operationDto.getCode());
        identifiersEditionForm.setValue(OperationDS.OP_CODE_VIEW, operationDto.getCode());

        title.setValue(operationDto.getTitle());
        acronym.setValue(operationDto.getAcronym());

        // Content classifiers
        subjectItem.clearValue();
        // subjectItem.setItemValue(operationDto.getSubjectArea() != null ? operationDto.getSubjectArea().getCodeId() : "");
        secondarySubjectItem.clearValue();
        // secondarySubjectItem.setItemsValues(ExternalItemUtils.getExternalItemsCodeIds(operationDto.getSecundarySubjectAreas()));

        // Content Descriptors
        description.setValue(operationDto.getDescription());
        objective.setValue(operationDto.getObjective());

        // Class Descriptors
        surveyType.setValue(operationDto.getSurveyType() != null ? operationDto.getSurveyType().getId() : null);
        officialityType.setValue(operationDto.getOfficialityType() != null ? operationDto.getOfficialityType().getId() : null);
        indSystem.setValue(operationDto.getIndicatorSystem() == null ? false : operationDto.getIndicatorSystem());

        // Production descriptors
        producerItem.clearValue();
        regionalResponsibleItem.clearValue();
        regionalContributorItem.getValue();
        productionEditionForm.setValue(OperationDS.OP_INTERNAL_INVENTORY_DATE, operationDto.getInternalInventoryDate());
        currentlyActiveItem.setValue(operationDto.getCurrentlyActive() != null ? operationDto.getCurrentlyActive() : false);
        statusItem.setValue(operationDto.getStatus() == null ? null : operationDto.getStatus().toString());
        productionEditionForm.setValue(OperationDS.OP_PROC_STATUS, getCoreMessages().getString(getCoreMessages().procStatusEnum() + operationDto.getProcStatus().getName()));
        productionEditionForm.setValue(OperationDS.OP_PROC_STATUS_VIEW, operationDto.getProcStatus().toString());

        // Diffusion and Publication
        publisherItem.clearValue();
        diffusionEditionForm.setValue(OperationDS.OP_RE_POL_US_AC, org.siemac.metamac.web.common.client.utils.RecordUtils.getInternationalStringRecord(operationDto.getRelPolUsAc()));
        releaseCalendar.setValue(operationDto.getReleaseCalendar());
        releaseCalendarAccess.setValue(operationDto.getReleaseCalendarAccess());
        updateFrequencyItem.setValues(ExternalItemUtils.getExternalItemsUrns(operationDto.getUpdateFrequency()));
        diffusionEditionForm.setValue(OperationDS.OP_CURRENT_INSTANCE, operationDto.getCurrentInstance() != null ? operationDto.getCurrentInstance().getCode() : "");
        diffusionEditionForm.setValue(OperationDS.OP_INVENTORY_DATE, operationDto.getInventoryDate());
        diffusionEditionForm.setValue(OperationDS.OP_REV_POLICY, org.siemac.metamac.web.common.client.utils.RecordUtils.getInternationalStringRecord(operationDto.getRevPolicy()));
        diffusionEditionForm.setValue(OperationDS.OP_REV_PRACTICE, org.siemac.metamac.web.common.client.utils.RecordUtils.getInternationalStringRecord(operationDto.getRevPractice()));
        commonMetadataItem.setValue(operationDto.getCommonMetadata() != null ? operationDto.getCommonMetadata().getUrn() : null);

        // Annotations
        annotationsEditionForm.setValue(OperationDS.OP_COMMENTS, org.siemac.metamac.web.common.client.utils.RecordUtils.getInternationalStringRecord(operationDto.getComment()));
        annotationsEditionForm.setValue(OperationDS.OP_NOTES, org.siemac.metamac.web.common.client.utils.RecordUtils.getInternationalStringRecord(operationDto.getNotes()));

        identifiersEditionForm.markForRedraw();
        productionEditionForm.markForRedraw();
    }

    @Override
    public HasClickHandlers getPublishOperationInternally() {
        return mainFormLayout.getPublishInternally();
    }

    @Override
    public HasClickHandlers getPublishOperationExternally() {
        return mainFormLayout.getPublishExternally();
    }

    /**
     * Select Instance in ListGrid
     * 
     * @param id
     */
    private void selectInstance(Long id) {
        if (id == null) {
            // New instance
            instanceListGridToolStrip.getDeleteButton().hide();
            instanceListGrid.deselectAllRecords();
        } else {
            showInstanceListGridDeleteButton();
        }
    }

    /**
     * DeSelect Instance in ListGrid
     */
    private void deselectInstance() {
        instanceListGridToolStrip.getDeleteButton().hide();
    }

    @Override
    public void setSubjects(List<ExternalItemDto> subjects) {
        this.subjects = subjects;
        subjectItem.setItemsValueMap(ExternalItemUtils.getExternalItemsHashMap(subjects));
    }

    @Override
    public void setSecondarySubjetcs(List<ExternalItemDto> secondarySubjects) {
        this.secondarySubjects = secondarySubjects;
        secondarySubjectItem.setItemsValueMap(ExternalItemUtils.getExternalItemsHashMap(secondarySubjects));
    }

    @Override
    public void setCategorySchemes(List<ExternalItemDto> schemes) {
        LinkedHashMap<String, String> map = ExternalItemUtils.getExternalItemsHashMap(schemes);
        subjectItem.setSchemesValueMap(map);
        secondarySubjectItem.setSchemesValueMap(map);
    }

    @Override
    public void setOrganisationSchemes(List<ExternalItemDto> schemes) {
        producerItem.setSchemesValueMap(ExternalItemUtils.getExternalItemsHashMap(schemes));
        regionalResponsibleItem.setSchemesValueMap(ExternalItemUtils.getExternalItemsHashMap(schemes));
        regionalContributorItem.setSchemesValueMap(ExternalItemUtils.getExternalItemsHashMap(schemes));
        publisherItem.setSchemesValueMap(ExternalItemUtils.getExternalItemsHashMap(schemes));
    }

    @Override
    public void setOperationsLists(List<SurveyTypeDto> surveyTypeDtos, List<OfficialityTypeDto> officialityTypeDtos) {
        this.surveyTypeDtos = surveyTypeDtos;
        this.officialityTypeDtos = officialityTypeDtos;
        surveyType.setValueMap(OperationsListUtils.getSurveyTypeHashMap(surveyTypeDtos));
        officialityType.setValueMap(OperationsListUtils.getOfficialityTypeHashMap(officialityTypeDtos));
    }

    @Override
    public void setProducers(List<ExternalItemDto> organisations) {
        this.producers = organisations;
        producerItem.setItemsValueMap(ExternalItemUtils.getExternalItemsHashMap(organisations));
    }

    @Override
    public void setRegionalResposibles(List<ExternalItemDto> organisations) {
        this.regionalResponsibles = organisations;
        regionalResponsibleItem.setItemsValueMap(ExternalItemUtils.getExternalItemsHashMap(organisations));
    }

    @Override
    public void setRegionalContributors(List<ExternalItemDto> organisations) {
        this.regionalContributors = organisations;
        regionalContributorItem.setItemsValueMap(ExternalItemUtils.getExternalItemsHashMap(organisations));
    }

    @Override
    public void setPublishers(List<ExternalItemDto> organisations) {
        this.publishers = organisations;
        publisherItem.setItemsValueMap(ExternalItemUtils.getExternalItemsHashMap(organisations));
    }

    @Override
    public void setCommonMetadataConfigurations(List<ExternalItemDto> commonMetadataList) {
        this.commonMetadataList = commonMetadataList;
        commonMetadataItem.setValueMap(ExternalItemUtils.getExternalItemsHashMap(commonMetadataList));
    }

    @Override
    public void setUpdateFrequencyCodes(List<ExternalItemDto> codes) {
        this.updateFrequencyCodes = codes;
        updateFrequencyItem.setValueMap(ExternalItemUtils.getExternalItemsHashMap(codes));
    }

    private void setTranslationsShowed(boolean translationsShowed) {
        // Set translationsShowed value to international fields
        identifiersViewForm.setTranslationsShowed(translationsShowed);
        identifiersEditionForm.setTranslationsShowed(translationsShowed);
        contentViewForm.setTranslationsShowed(translationsShowed);
        contentEditionForm.setTranslationsShowed(translationsShowed);
        diffusionViewForm.setTranslationsShowed(translationsShowed);
        diffusionEditionForm.setTranslationsShowed(translationsShowed);
        annotationsViewForm.setTranslationsShowed(translationsShowed);
        annotationsEditionForm.setTranslationsShowed(translationsShowed);
    }

    private void showInstanceListGridDeleteButton() {
        if (ClientSecurityUtils.canDeleteInstance(operationDto.getCode())) {
            instanceListGridToolStrip.getDeleteButton().show();
        }
    }

    private boolean canOperationCodeBeEdited() {
        // Operation code can be edited only when ProcStatus is DRAFT
        return (productionEditionForm.getValue(OperationDS.OP_PROC_STATUS_VIEW) != null && ProcStatusEnum.DRAFT.toString().equals(productionEditionForm.getValue(OperationDS.OP_PROC_STATUS_VIEW)));
    }

    public boolean isOperationInternallyPublished() {
        return ProcStatusEnum.PUBLISH_INTERNALLY.equals(operationDto.getProcStatus());
    }

    public boolean isOperationExternallyPublished() {
        return ProcStatusEnum.PUBLISH_EXTERNALLY.equals(operationDto.getProcStatus());
    }

    @Override
    public void setUiHandlers(OperationUiHandlers uiHandlers) {
        this.uiHandlers = uiHandlers;
    }

    @Override
    public void setFamilies(List<FamilyBaseDto> familyBaseDtos, int firstResult, int totalResults) {
        addFamiliesToOperationWindow.setFamilies(familyBaseDtos, firstResult, totalResults);
    }

}
