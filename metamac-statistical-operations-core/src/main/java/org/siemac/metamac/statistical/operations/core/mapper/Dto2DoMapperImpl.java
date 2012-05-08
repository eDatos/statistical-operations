package org.siemac.metamac.statistical.operations.core.mapper;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.fornax.cartridges.sculptor.framework.errorhandling.ServiceContext;
import org.joda.time.DateTime;
import org.siemac.metamac.core.common.bt.domain.ExternalItemBt;
import org.siemac.metamac.core.common.dto.ExternalItemBtDto;
import org.siemac.metamac.core.common.dto.InternationalStringDto;
import org.siemac.metamac.core.common.dto.LocalisedStringDto;
import org.siemac.metamac.core.common.ent.domain.InternationalString;
import org.siemac.metamac.core.common.ent.domain.InternationalStringRepository;
import org.siemac.metamac.core.common.ent.domain.LocalisedString;
import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.core.common.exception.MetamacExceptionItem;
import org.siemac.metamac.core.common.exception.utils.ExceptionUtils;
import org.siemac.metamac.core.common.serviceimpl.utils.ValidationUtils;
import org.siemac.metamac.core.common.util.OptimisticLockingUtils;
import org.siemac.metamac.core.common.vo.domain.ExternalItem;
import org.siemac.metamac.core.common.vo.domain.ExternalItemRepository;
import org.siemac.metamac.domain.statistical.operations.dto.CollMethodDto;
import org.siemac.metamac.domain.statistical.operations.dto.CostDto;
import org.siemac.metamac.domain.statistical.operations.dto.FamilyDto;
import org.siemac.metamac.domain.statistical.operations.dto.InstanceDto;
import org.siemac.metamac.domain.statistical.operations.dto.InstanceTypeDto;
import org.siemac.metamac.domain.statistical.operations.dto.OfficialityTypeDto;
import org.siemac.metamac.domain.statistical.operations.dto.OperationDto;
import org.siemac.metamac.domain.statistical.operations.dto.SurveySourceDto;
import org.siemac.metamac.domain.statistical.operations.dto.SurveyTypeDto;
import org.siemac.metamac.domain.statistical.operations.enume.domain.ProcStatusEnum;
import org.siemac.metamac.statistical.operations.core.domain.CollMethod;
import org.siemac.metamac.statistical.operations.core.domain.Cost;
import org.siemac.metamac.statistical.operations.core.domain.Family;
import org.siemac.metamac.statistical.operations.core.domain.Instance;
import org.siemac.metamac.statistical.operations.core.domain.InstanceType;
import org.siemac.metamac.statistical.operations.core.domain.OfficialityType;
import org.siemac.metamac.statistical.operations.core.domain.Operation;
import org.siemac.metamac.statistical.operations.core.domain.SurveySource;
import org.siemac.metamac.statistical.operations.core.domain.SurveyType;
import org.siemac.metamac.statistical.operations.core.error.ServiceExceptionParameters;
import org.siemac.metamac.statistical.operations.core.error.ServiceExceptionType;
import org.siemac.metamac.statistical.operations.core.serviceapi.StatisticalOperationsBaseService;
import org.siemac.metamac.statistical.operations.core.serviceapi.StatisticalOperationsListsService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class Dto2DoMapperImpl implements Dto2DoMapper {

    @Autowired
    private StatisticalOperationsListsService statisticalOperationsListsService;

    @Autowired
    private StatisticalOperationsBaseService  statisticalOperationsBaseService;

    @Autowired
    private InternationalStringRepository     internationalStringRepository;

    @Autowired
    private ExternalItemRepository            externalItemRepository;

    /**************************************************************************
     * PUBLIC - LISTS
     **************************************************************************/

    @Override
    public SurveyType surveyTypeDtoToEntity(SurveyTypeDto source, ServiceContext ctx) throws MetamacException {
        if (source == null) {
            return null;
        }

        SurveyType target = statisticalOperationsListsService.findSurveyTypeById(ctx, source.getId());

        return target;
    }

    @Override
    public InstanceType instanceTypeDtoToEntity(InstanceTypeDto source, ServiceContext ctx) throws MetamacException {
        if (source == null) {
            return null;
        }

        InstanceType target = statisticalOperationsListsService.findInstanceTypeById(ctx, source.getId());

        return target;
    }

    @Override
    public SurveySource surveySourceDtoToEntity(SurveySourceDto source, ServiceContext ctx) throws MetamacException {
        if (source == null) {
            return null;
        }

        SurveySource target = statisticalOperationsListsService.findSurveySourceById(ctx, source.getId());

        return target;
    }

    @Override
    public OfficialityType officialityTypeDtoToEntity(OfficialityTypeDto source, ServiceContext ctx) throws MetamacException {
        if (source == null) {
            return null;
        }

        OfficialityType target = statisticalOperationsListsService.findOfficialityTypeById(ctx, source.getId());

        return target;
    }

    @Override
    public CollMethod collMethodDtoToEntity(CollMethodDto source, ServiceContext ctx) throws MetamacException {
        if (source == null) {
            return null;
        }

        CollMethod target = statisticalOperationsListsService.findCollMethodById(ctx, source.getId());

        return target;
    }

    @Override
    public Cost costDtoToEntity(CostDto source, ServiceContext ctx) throws MetamacException {
        if (source == null) {
            return null;
        }

        Cost target = statisticalOperationsListsService.findCostById(ctx, source.getId());

        return target;
    }

    /**************************************************************************
     * PUBLIC - ENTITIES
     **************************************************************************/

    @Override
    public Family familyDtoToEntity(FamilyDto source, ServiceContext ctx) throws MetamacException {
        if (source == null) {
            return null;
        }

        // If exists, retrieves existing entity. Otherwise, creates new entity
        Family target = new Family();
        if (source.getId() != null) {
            target = statisticalOperationsBaseService.findFamilyById(ctx, source.getId());
            OptimisticLockingUtils.checkVersion(target.getVersion(), source.getOptimisticLockingVersion());

            // Metadata unmodifiable
            if (!ProcStatusEnum.DRAFT.equals(target.getProcStatus())) {
                List<MetamacExceptionItem> exceptions = new ArrayList<MetamacExceptionItem>();
                ValidationUtils.checkMetadataUnmodifiable(target.getCode(), source.getCode(), ServiceExceptionParameters.FAMILY_CODE, exceptions);
                ExceptionUtils.throwIfException(exceptions);
            }
        }

        target = familyDtoToEntity(source, target);
        return target;
    }

    @Override
    public Operation operationDtoToEntity(OperationDto source, ServiceContext ctx) throws MetamacException {
        if (source == null) {
            return null;
        }

        // If exists, retrieves existing entity. Otherwise, creates new entity
        Operation target = new Operation();
        if (source.getId() != null) {
            target = statisticalOperationsBaseService.findOperationById(ctx, source.getId());
            OptimisticLockingUtils.checkVersion(target.getVersion(), source.getOptimisticLockingVersion());

            // Metadata unmodifiable
            if (!ProcStatusEnum.DRAFT.equals(target.getProcStatus())) {
                List<MetamacExceptionItem> exceptions = new ArrayList<MetamacExceptionItem>();
                ValidationUtils.checkMetadataUnmodifiable(target.getCode(), source.getCode(), ServiceExceptionParameters.OPERATION_CODE, exceptions);
                ExceptionUtils.throwIfException(exceptions);
            }
        }

        target = operationDtoToEntity(source, target, ctx);
        return target;
    }

    @Override
    public Instance instanceDtoToEntity(InstanceDto source, ServiceContext ctx) throws MetamacException {
        if (source == null) {
            return null;
        }

        // If exists, retrieves existing entity. Otherwise, creates new entity
        Instance target = new Instance();
        if (source.getId() != null) {
            target = statisticalOperationsBaseService.findInstanceById(ctx, source.getId());
            OptimisticLockingUtils.checkVersion(target.getVersion(), source.getOptimisticLockingVersion());

            // Metadata unmodifiable
            if (!ProcStatusEnum.DRAFT.equals(target.getProcStatus())) {
                List<MetamacExceptionItem> exceptions = new ArrayList<MetamacExceptionItem>();
                ValidationUtils.checkMetadataUnmodifiable(target.getCode(), source.getCode(), ServiceExceptionParameters.INSTANCE_CODE, exceptions);
                ExceptionUtils.throwIfException(exceptions);
            }
        }

        target = instanceDtoToEntity(source, target, ctx);
        return target;
    }

    /**************************************************************************
     * PRIVATE
     **************************************************************************/

    private Family familyDtoToEntity(FamilyDto source, Family target) throws MetamacException {
        if (target == null) {
            throw new MetamacException(ServiceExceptionType.PARAMETER_REQUIRED);
        }

        // CODE
        target.setCode(source.getCode());

        // TITLE
        target.setTitle(internationalStringToEntity(source.getTitle(), target.getTitle(), ServiceExceptionParameters.FAMILY_TITLE));

        // ACRONYM
        target.setAcronym(internationalStringToEntity(source.getAcronym(), target.getAcronym(), ServiceExceptionParameters.FAMILY_ACRONYM));

        // DESCRIPTION
        target.setDescription(internationalStringToEntity(source.getDescription(), target.getDescription(), ServiceExceptionParameters.FAMILY_DESCRIPTION));

        // SURVEY_CODE
        // Not necessary. It's a relation

        // SURVEY_TITLE
        // Not necessary. It's a relation

        // INTERNAL_INVENTORY_DATE
        // Not necessary. It can't be manually modified

        // PROC STATUS
        // Not necessary. It can't be manually modified

        // INVENTORY_DATE
        // Not necessary. It can't be manually modified

        // Optimistic locking: Update "update date" attribute to force update of the root entity in order to increase attribute "version"
        target.setUpdateDate(new DateTime());

        return target;
    }

    private Operation operationDtoToEntity(OperationDto source, Operation target, ServiceContext ctx) throws MetamacException {
        if (target == null) {
            throw new MetamacException(ServiceExceptionType.PARAMETER_REQUIRED);
        }

        // APP_COMMON_METADATA
        target.setCommonMetadata(externalItemBtDtoToExternalItemBt(source.getCommonMetadata(), target.getCommonMetadata()));

        // CODE
        target.setCode(source.getCode());

        // TITLE
        target.setTitle(internationalStringToEntity(source.getTitle(), target.getTitle(), ServiceExceptionParameters.OPERATION_TITLE));

        // TITLE_ALETERNATIVE
        target.setAcronym(internationalStringToEntity(source.getAcronym(), target.getAcronym(), ServiceExceptionParameters.OPERATION_ACRONYM));

        // FAMILY_CODE
        // Not necessary.It's a relation

        // FAMILY_TITLE
        // Not necessary. It's a relation

        // SUBJECT_AREA
        target.setSubjectArea(externalItemBtDtoToExternalItemBt(source.getSubjectArea(), target.getSubjectArea()));

        // SECONDARY_SUBJECT_AREAS
        target.getSecondarySubjectAreas().clear();
        target.getSecondarySubjectAreas().addAll(externalItemListToEntity(source.getSecondarySubjectAreas(), target.getSecondarySubjectAreas()));

        // OBJECTIVE
        target.setObjective(internationalStringToEntity(source.getObjective(), target.getObjective(), ServiceExceptionParameters.OPERATION_OBJECTIVE));

        // DESCRIPTION
        target.setDescription(internationalStringToEntity(source.getDescription(), target.getDescription(), ServiceExceptionParameters.OPERATION_DESCRIPTION));

        // INSTANCE_CODE
        // Not necessary. It's a relation

        // INSTANCE_TITLE
        // Not necessary. It's a relation

        // SURVEY_TYPE
        target.setSurveyType(surveyTypeDtoToEntity(source.getSurveyType(), ctx));

        // INDICATOR_SYSTEM
        target.setIndicatorSystem(source.getIndicatorSystem());

        // OFFICIALITY_TYPE
        target.setOfficialityType(officialityTypeDtoToEntity(source.getOfficialityType(), ctx));

        // PRODUCER
        target.getProducer().clear();
        target.getProducer().addAll(externalItemListToEntity(source.getProducer(), target.getProducer()));

        // REGIONAL_RESPONSIBLE
        target.getRegionalResponsible().clear();
        target.getRegionalResponsible().addAll(externalItemListToEntity(source.getRegionalResponsible(), target.getRegionalResponsible()));

        // REGIONAL_CONTRIBUTOR
        target.getRegionalContributor().clear();
        target.getRegionalContributor().addAll(externalItemListToEntity(source.getRegionalContributor(), target.getRegionalContributor()));

        // INTERNAL_INVENTORY_DATE
        // Not necessary. It can't be manually modified

        // CURRENTLY_ACTIVE
        target.setCurrentlyActive(source.getCurrentlyActive());

        // INACTIVE_DATE
        // Not necessary. It can't be manually modified

        // STATUS
        target.setStatus(source.getStatus());

        // PROC_STATUS
        // Not necessary. It can't be manually modified

        // PUBLISHER
        target.getPublisher().clear();
        target.getPublisher().addAll(externalItemListToEntity(source.getPublisher(), target.getPublisher()));

        // REL_POL_US_AC
        target.setRelPolUsAc(internationalStringToEntity(source.getRelPolUsAc(), target.getRelPolUsAc(), ServiceExceptionParameters.OPERATION_REL_POL_US_AC));

        // REL_POL_US_AC_URL
        target.setRelPolUsAcUrl(source.getRelPolUsAcUrl());

        // RELEASE_CALENDAR
        target.setReleaseCalendar(source.getReleaseCalendar());

        // RELEASE_CALENDAR_ACCESS
        target.setReleaseCalendarAccess(source.getReleaseCalendarAccess());

        // UPDATE_FREQUENCY
        target.getUpdateFrequency().clear();
        target.getUpdateFrequency().addAll(externalItemListToEntity(source.getUpdateFrequency(), target.getUpdateFrequency()));

        // CURRENT_INTERNAL_INSTANCE
        // Not necessary. Extracted from instances order.

        // CURRENT_INSTANCE.
        // Not necessary. Extracted from instances order.

        // INVENTORY_DATE
        // Not necessary. It can't be manually modified.

        // REV_POLICY
        target.setRevPolicy(internationalStringToEntity(source.getRevPolicy(), target.getRevPolicy(), ServiceExceptionParameters.OPERATION_REV_POLICY));

        // REV_POLICY_URL
        target.setRevPolicyUrl(source.getRevPolicyUrl());

        // REV_PRACTICE
        target.setRevPractice(internationalStringToEntity(source.getRevPractice(), target.getRevPractice(), ServiceExceptionParameters.OPERATION_REV_PRACTICE));

        // REV_PRACTICE_URL
        target.setRevPracticeUrl(source.getRevPracticeUrl());

        // CONTACT: Extracted from AppCommonMetadata

        // LEGAL_ACTS: Extracted from AppCommonMetadata
        // DATA_SHARING: Extracted from AppCommonMetadata
        // CONFIDENCIALITY_POLICY: Extracted from AppCommonMetadata
        // CONFIDENCIALITY_DATA_TREATMENT: Extracted from AppCommonMetadata

        // COMMENT
        target.setComment(internationalStringToEntity(source.getComment(), target.getComment(), ServiceExceptionParameters.OPERATION_COMMENT));

        // COMMENT_URL
        target.setCommentUrl(source.getCommentUrl());

        // NOTES
        target.setNotes(internationalStringToEntity(source.getNotes(), target.getNotes(), ServiceExceptionParameters.OPERATION_NOTES));

        // NOTES_URL
        target.setNotesUrl(source.getNotesUrl());

        // Optimistic locking: Update "update date" attribute to force update of the root entity in order to increase attribute "version"
        target.setUpdateDate(new DateTime());

        return target;
    }

    private Instance instanceDtoToEntity(InstanceDto source, Instance target, ServiceContext ctx) throws MetamacException {
        if (target == null) {
            throw new MetamacException(ServiceExceptionType.PARAMETER_REQUIRED);
        }

        // ORDER
        // Not necessary. It can't be manually edited.

        // CODE
        target.setCode(source.getCode());

        // TITLE
        target.setTitle(internationalStringToEntity(source.getTitle(), target.getTitle(), ServiceExceptionParameters.INSTANCE_TITLE));

        // ACRONYM
        target.setAcronym(internationalStringToEntity(source.getAcronym(), target.getAcronym(), ServiceExceptionParameters.INSTANCE_ACRONYM));

        // SURVEY_CODE
        // Not necessary. It's a relation

        // SURVEY_TITLE
        // Not necessary. It's a relation

        // SUCCESSOR
        // Not necessary. Extracted from instances order.

        // PREDECESSOR
        // Not necessary. Extracted from instances order.

        // DATA_DESCRIPTION
        target.setDataDescription(internationalStringToEntity(source.getDataDescription(), target.getDataDescription(), ServiceExceptionParameters.INSTANCE_DESCRIPTION));

        // STATISTICAL_POPULATION
        target.setStatisticalPopulation(internationalStringToEntity(source.getStatisticalPopulation(), target.getStatisticalPopulation(), ServiceExceptionParameters.INSTANCE_STATISTICAL_POPULATION));

        // STATISTICAL_UNIT
        target.getStatisticalUnit().clear();
        target.getStatisticalUnit().addAll(externalItemListToEntity(source.getStatisticalUnit(), target.getStatisticalUnit()));

        // GEOGRAPHIC_GRANULARITY
        target.setGeographicGranularity(externalItemBtDtoToExternalItemBt(source.getGeographicGranularity(), target.getGeographicGranularity()));

        // GEOGRAPHIC_COMPARABILITY
        target.setGeographicComparability(internationalStringToEntity(source.getGeographicComparability(), target.getGeographicComparability(),
                ServiceExceptionParameters.INSTANCE_GEOGRAPHIC_COMPARABILITY));

        // TEMPORAL_GRANULARITY
        target.setTemporalGranularity(externalItemBtDtoToExternalItemBt(source.getTemporalGranularity(), target.getTemporalGranularity()));

        // TEMPORAL_COMPARABILITY
        target.setTemporalComparability(internationalStringToEntity(source.getTemporalComparability(), target.getTemporalComparability(), ServiceExceptionParameters.INSTANCE_TEMPORAL_COMPARABILITY));

        // BASE_PERIOD
        target.setBasePeriod(source.getBasePeriod());

        // UNIT_MEASURE
        target.getUnitMeasure().clear();
        target.getUnitMeasure().addAll(externalItemListToEntity(source.getUnitMeasure(), target.getUnitMeasure()));

        // STAT_CONC_DEF
        target.setStatConcDef(internationalStringToEntity(source.getStatConcDef(), target.getStatConcDef(), ServiceExceptionParameters.INSTANCE_STAT_CONC_DEF));

        // STAT_CONC_DEF_LIST
        target.getStatConcDefList().clear();
        target.getStatConcDefList().addAll(externalItemListToEntity(source.getStatConcDefList(), target.getStatConcDefList()));

        // CLASS_SYSTEM
        target.setClassSystem(internationalStringToEntity(source.getClassSystem(), target.getClassSystem(), ServiceExceptionParameters.INSTANCE_CLASS_SYSTEM));

        // CLASS_SYSTEM_LIST
        target.getClassSystemList().clear();
        target.getClassSystemList().addAll(externalItemListToEntity(source.getClassSystemList(), target.getClassSystemList()));

        // INSTANCE_TYPE
        target.setInstanceType(instanceTypeDtoToEntity(source.getInstanceType(), ctx));

        // INTERNAL_INVENTORY_DATE
        // Not necessary. It can't be manually edited.

        // PROC_STATUS
        // Not necessary. It can't be manually edited.

        // DOC_METHOD
        target.setDocMethod(internationalStringToEntity(source.getDocMethod(), target.getDocMethod(), ServiceExceptionParameters.INSTANCE_DOC_METHOD));

        // DOC_METHOD_URL
        target.setDocMethodUrl(source.getDocMethodUrl());

        // SURVEY_SOURCE
        target.setSurveySource(surveySourceDtoToEntity(source.getSurveySource(), ctx));

        // COLL_METHOD
        target.setCollMethod(collMethodDtoToEntity(source.getCollMethod(), ctx));

        // INFORMATION_SUPPLIERS
        target.getInformationSuppliers().clear();
        target.getInformationSuppliers().addAll(externalItemListToEntity(source.getInformationSuppliers(), target.getInformationSuppliers()));

        // FREQ_COLL
        target.getFreqColl().clear();
        target.getFreqColl().addAll(externalItemListToEntity(source.getFreqColl(), target.getFreqColl()));

        // DATA_VALIDATION
        target.setDataValidation(internationalStringToEntity(source.getDataValidation(), target.getDataValidation(), ServiceExceptionParameters.INSTANCE_DATA_VALIDATION));

        // DATA_VALIDATION_URL
        target.setDataValidationUrl(source.getDataValidationUrl());

        // DATA_COMPILATION
        target.setDataCompilation(internationalStringToEntity(source.getDataCompilation(), target.getDataCompilation(), ServiceExceptionParameters.INSTANCE_DATA_COMPILATION));

        // DATA_COMPILATION_URL
        target.setDataCompilationUrl(source.getDataCompilationUrl());

        // ADJUSTMENT
        target.setAdjustment(internationalStringToEntity(source.getAdjustment(), target.getAdjustment(), ServiceExceptionParameters.INSTANCE_ADJUSTMENT));

        // ADJUSTMENT_URL
        target.setAdjustmentUrl(source.getAdjustmentUrl());

        // COST_BURDEN
        target.setCostBurden(internationalStringToEntity(source.getCostBurden(), target.getCostBurden(), ServiceExceptionParameters.INSTANCE_COST_BURDEN));

        // COST_BURDEN_URL
        target.setCostBurdenUrl(source.getCostBurdenUrl());

        // COST
        target.getCost().addAll(costDtoListToCostList(source.getCost(), target.getCost(), ctx));

        // INVENTORY_DATE
        // Not necessary. It can't be manually edited.

        // QUALITY_DOC
        target.setQualityDoc(internationalStringToEntity(source.getQualityDoc(), target.getQualityDoc(), ServiceExceptionParameters.INSTANCE_QUALITY_DOC));

        // QUALITY_DOC_URL
        target.setQualityDocUrl(source.getQualityDocUrl());

        // QUALITY_ASSURE
        target.setQualityAssure(internationalStringToEntity(source.getQualityAssure(), target.getQualityAssure(), ServiceExceptionParameters.INSTANCE_QUALITY_ASSURE));

        // QUALITY_ASSURE_URL
        target.setQualityAssureUrl(source.getQualityAssureUrl());

        // QUALITY_ASSMNT
        target.setQualityAssmnt(internationalStringToEntity(source.getQualityAssmnt(), target.getQualityAssmnt(), ServiceExceptionParameters.INSTANCE_QUALITY_ASSMNT));

        // QUALITY_ASSMNT_URL
        target.setQualityAssmntUrl(source.getQualityAssmntUrl());

        // USER_NEEDS
        target.setUserNeeds(internationalStringToEntity(source.getUserNeeds(), target.getUserNeeds(), ServiceExceptionParameters.INSTANCE_USER_NEEDS));

        // USER_NEEDS_URL
        target.setUserNeedsUrl(source.getUserNeedsUrl());

        // USER_SAT
        target.setUserSat(internationalStringToEntity(source.getUserSat(), target.getUserSat(), ServiceExceptionParameters.INSTANCE_USER_SAT));

        // USER_SAT_URL
        target.setUserSatUrl(source.getUserSatUrl());

        // COMPLETENESS
        target.setCompleteness(internationalStringToEntity(source.getCompleteness(), target.getCompleteness(), ServiceExceptionParameters.INSTANCE_COMPLETENESS));

        // COMPLETENESS_URL
        target.setCompletenessUrl(source.getCompletenessUrl());

        // TIMELINESS
        target.setTimeliness(internationalStringToEntity(source.getTimeliness(), target.getTimeliness(), ServiceExceptionParameters.INSTANCE_TIMELINESS));

        // TIMELINESS_URL
        target.setTimelinessUrl(source.getTimelinessUrl());

        // PUNCTUALITY
        target.setPunctuality(internationalStringToEntity(source.getPunctuality(), target.getPunctuality(), ServiceExceptionParameters.INSTANCE_PUNCTUALITY));

        // PUNCTUALITY_URL
        target.setPunctualityUrl(source.getPunctualityUrl());

        // ACCURACY_OVERALL
        target.setAccuracyOverall(internationalStringToEntity(source.getAccuracyOverall(), target.getAccuracyOverall(), ServiceExceptionParameters.INSTANCE_ACCURACY_OVERALL));

        // ACCURACY_OVERALL_URL
        target.setAccuracyOverallUrl(source.getAccuracyOverallUrl());

        // SAMPLING_ERR
        target.setSamplingErr(internationalStringToEntity(source.getSamplingErr(), target.getSamplingErr(), ServiceExceptionParameters.INSTANCE_SAMPLING_ERR));

        // SAMPLING_ERR_URL
        target.setSamplingErrUrl(source.getSamplingErrUrl());

        // NONSAMPLING_ERR
        target.setNonsamplingErr(internationalStringToEntity(source.getNonsamplingErr(), target.getNonsamplingErr(), ServiceExceptionParameters.INSTANCE_NONSAMPLING_ERR));

        // NONSAMPLING_ERR_URL
        target.setNonsamplingErrUrl(source.getNonsamplingErrUrl());

        // COHER_X_DOMAIN
        target.setCoherXDomain(internationalStringToEntity(source.getCoherXDomain(), target.getCoherXDomain(), ServiceExceptionParameters.INSTANCE_COHER_X_DOMAIN));

        // COHER_X_DOMAIN_URL
        target.setCoherXDomainUrl(source.getCoherXDomainUrl());

        // COHER_INTERNAL
        target.setCoherInternal(internationalStringToEntity(source.getCoherInternal(), target.getCoherInternal(), ServiceExceptionParameters.INSTANCE_COHER_INTERNAL));

        // COHER_INTERNAL_URL
        target.setCoherInternalUrl(source.getCoherInternalUrl());

        // COMMENT
        target.setComment(internationalStringToEntity(source.getComment(), target.getComment(), ServiceExceptionParameters.INSTANCE_COMMENT));

        // COMMENT_URL
        target.setCommentUrl(source.getCommentUrl());

        // NOTES
        target.setNotes(internationalStringToEntity(source.getNotes(), target.getNotes(), ServiceExceptionParameters.INSTANCE_NOTES));

        // NOTES_URL
        target.setNotesUrl(source.getNotesUrl());

        // Optimistic locking: Update "update date" attribute to force update of the root entity in order to increase attribute "version"
        target.setUpdateDate(new DateTime());

        return target;
    }

    private Set<ExternalItem> externalItemListToEntity(Set<ExternalItemBtDto> source, Set<ExternalItem> target) {

        if (source.isEmpty()) {
            if (!target.isEmpty()) {
                // Delete old entity
                deleteExternalItemList(target);
            }
            return new HashSet<ExternalItem>();
        }

        if (target.isEmpty()) {
            target = new HashSet<ExternalItem>();
        }

        Set<ExternalItem> targetsBefore = target;
        target.clear();

        for (ExternalItemBtDto itemDto : source) {
            boolean existsBefore = false;
            for (ExternalItem item : targetsBefore) {
                if (checkExternalItem(itemDto, item)) {
                    existsBefore = true;
                    target.add(item);
                    break;
                }
            }

            if (!existsBefore) {
                target.add(externalItemBtDtoToExternalItem(itemDto));
            }
        }

        return target;
    }

    private ExternalItem externalItemBtDtoToExternalItem(ExternalItemBtDto source) {
        if (source == null) {
            return null;
        }

        ExternalItem target = new ExternalItem(new ExternalItemBt(source.getUriInt(), source.getCodeId(), source.getType()));

        return target;
    }

    private ExternalItemBt externalItemBtDtoToExternalItemBt(ExternalItemBtDto source, ExternalItemBt target) {
        if (source == null) {
            return null;
        } else {
            target = new ExternalItemBt(source.getUriInt(), source.getCodeId(), source.getType());
        }

        return target;
    }

    private boolean checkExternalItem(ExternalItemBtDto itemDto, ExternalItem item) {
        return (itemDto.getCodeId().equals(item.getExt().getCodeId()) && itemDto.getType().equals(item.getExt().getType()) && itemDto.getType().equals(item.getExt().getType()));
    }

    /**
     * Delete the externalItem of an externalItemList
     * 
     * @param target
     */
    private void deleteExternalItemList(Set<ExternalItem> target) {
        for (ExternalItem externalItem : target) {
            externalItemRepository.delete(externalItem);
        }

    }

    /**
     * Transfrom InternationString
     * 
     * @throws MetamacException
     */

    private InternationalString internationalStringToEntity(InternationalStringDto source, InternationalString target, String metadataName) throws MetamacException {

        if (source == null) {
            if (target != null) {
                // Delete old entity
                internationalStringRepository.delete(target);
            }

            return null;
        }

        if (target == null) {
            target = new InternationalString();
        }

        if (ValidationUtils.isEmpty(source)) {
            throw new MetamacException(ServiceExceptionType.METADATA_REQUIRED, metadataName);
        }

        Set<LocalisedString> localisedStringEntities = localisedStringDtoToDo(source.getTexts(), target.getTexts());
        target.getTexts().clear();
        target.getTexts().addAll(localisedStringEntities);

        return target;
    }

    /**
     * Transform LocalisedString, reusing existing locales
     */
    private Set<LocalisedString> localisedStringDtoToDo(Set<LocalisedStringDto> sources, Set<LocalisedString> targets) {

        Set<LocalisedString> targetsBefore = targets;
        targets = new HashSet<LocalisedString>();

        for (LocalisedStringDto source : sources) {
            boolean existsBefore = false;
            for (LocalisedString target : targetsBefore) {
                if (source.getLocale().equals(target.getLocale())) {
                    targets.add(localisedStringDtoToDo(source, target));
                    existsBefore = true;
                    break;
                }
            }
            if (!existsBefore) {
                targets.add(localisedStringDtoToDo(source));
            }
        }
        return targets;
    }

    private LocalisedString localisedStringDtoToDo(LocalisedStringDto source) {
        LocalisedString target = new LocalisedString();
        target.setLabel(source.getLabel());
        target.setLocale(source.getLocale());
        return target;
    }

    private LocalisedString localisedStringDtoToDo(LocalisedStringDto source, LocalisedString target) {
        target.setLabel(source.getLabel());
        target.setLocale(source.getLocale());
        return target;
    }

    private Set<Cost> costDtoListToCostList(Set<CostDto> source, Set<Cost> target, ServiceContext ctx) throws MetamacException {

        Set<Cost> result = new HashSet<Cost>();

        // Exist
        if (target != null) {
            for (CostDto itemDto : source) {
                boolean found = false;
                for (Cost itemPersisted : target) {
                    if (itemPersisted.equals(itemDto)) {
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    result.add(costDtoToEntity(itemDto, ctx));
                }
            }
        } else {
            // New
            target = new HashSet<Cost>();
            for (CostDto item : source) {
                result.add(costDtoToEntity(item, ctx));
            }
        }

        return result;
    }

}
