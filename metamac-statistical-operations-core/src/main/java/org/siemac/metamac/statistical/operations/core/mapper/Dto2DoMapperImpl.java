package org.siemac.metamac.statistical.operations.core.mapper;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.fornax.cartridges.sculptor.framework.errorhandling.ServiceContext;
import org.joda.time.DateTime;
import org.siemac.metamac.core.common.dto.ExternalItemDto;
import org.siemac.metamac.core.common.dto.InternationalStringDto;
import org.siemac.metamac.core.common.dto.LocalisedStringDto;
import org.siemac.metamac.core.common.ent.domain.ExternalItem;
import org.siemac.metamac.core.common.ent.domain.ExternalItemRepository;
import org.siemac.metamac.core.common.ent.domain.InternationalString;
import org.siemac.metamac.core.common.ent.domain.InternationalStringRepository;
import org.siemac.metamac.core.common.ent.domain.LocalisedString;
import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.core.common.exception.MetamacExceptionItem;
import org.siemac.metamac.core.common.exception.utils.ExceptionUtils;
import org.siemac.metamac.core.common.serviceimpl.utils.ValidationUtils;
import org.siemac.metamac.core.common.util.OptimisticLockingUtils;
import org.siemac.metamac.statistical.operations.core.domain.CollMethod;
import org.siemac.metamac.statistical.operations.core.domain.Cost;
import org.siemac.metamac.statistical.operations.core.domain.Family;
import org.siemac.metamac.statistical.operations.core.domain.Instance;
import org.siemac.metamac.statistical.operations.core.domain.InstanceType;
import org.siemac.metamac.statistical.operations.core.domain.OfficialityType;
import org.siemac.metamac.statistical.operations.core.domain.Operation;
import org.siemac.metamac.statistical.operations.core.domain.SurveySource;
import org.siemac.metamac.statistical.operations.core.domain.SurveyType;
import org.siemac.metamac.statistical.operations.core.dto.CollMethodDto;
import org.siemac.metamac.statistical.operations.core.dto.CostDto;
import org.siemac.metamac.statistical.operations.core.dto.FamilyDto;
import org.siemac.metamac.statistical.operations.core.dto.InstanceDto;
import org.siemac.metamac.statistical.operations.core.dto.InstanceTypeDto;
import org.siemac.metamac.statistical.operations.core.dto.OfficialityTypeDto;
import org.siemac.metamac.statistical.operations.core.dto.OperationDto;
import org.siemac.metamac.statistical.operations.core.dto.SurveySourceDto;
import org.siemac.metamac.statistical.operations.core.dto.SurveyTypeDto;
import org.siemac.metamac.statistical.operations.core.enume.domain.ProcStatusEnum;
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
            // It's necessary to check that all the metadata that conforms the URN are unmodifibale.
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
            // It's necessary to check that all the metadata that conforms the URN are unmodifibale.
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
            // It's necessary to check that all the metadata that conforms the URN are unmodifibale.
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

        // URN
        // Not necessary. It can't be manually modified

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
        target.setCommonMetadata(externalItemDtoToEntity(source.getCommonMetadata(), target.getCommonMetadata(), ServiceExceptionParameters.OPERATION_COMMON_METADATA));

        // CODE
        target.setCode(source.getCode());

        // URN
        // Not necessary. It can't be manually modified

        // TITLE
        target.setTitle(internationalStringToEntity(source.getTitle(), target.getTitle(), ServiceExceptionParameters.OPERATION_TITLE));

        // TITLE_ALETERNATIVE
        target.setAcronym(internationalStringToEntity(source.getAcronym(), target.getAcronym(), ServiceExceptionParameters.OPERATION_ACRONYM));

        // FAMILY_CODE
        // Not necessary.It's a relation

        // FAMILY_TITLE
        // Not necessary. It's a relation

        // SUBJECT_AREA
        target.setSubjectArea(externalItemDtoToEntity(source.getSubjectArea(), target.getSubjectArea(), ServiceExceptionParameters.OPERATION_SUBJECT_AREA));

        // SECONDARY_SUBJECT_AREAS
        target.getSecondarySubjectAreas().clear();
        target.getSecondarySubjectAreas().addAll(
                externalItemListToEntity(source.getSecondarySubjectAreas(), target.getSecondarySubjectAreas(), ServiceExceptionParameters.OPERATION_SECONDARY_SUBJECT_AREAS));

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
        target.getProducer().addAll(externalItemListToEntity(source.getProducer(), target.getProducer(), ServiceExceptionParameters.OPERATION_PRODUCER));

        // REGIONAL_RESPONSIBLE
        target.getRegionalResponsible().clear();
        target.getRegionalResponsible().addAll(externalItemListToEntity(source.getRegionalResponsible(), target.getRegionalResponsible(), ServiceExceptionParameters.OPERATION_REGIONAL_RESPONSIBLE));

        // REGIONAL_CONTRIBUTOR
        target.getRegionalContributor().clear();
        target.getRegionalContributor().addAll(externalItemListToEntity(source.getRegionalContributor(), target.getRegionalContributor(), ServiceExceptionParameters.OPERATION_REGIONAL_CONTRIBUTOR));

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
        target.getPublisher().addAll(externalItemListToEntity(source.getPublisher(), target.getPublisher(), ServiceExceptionParameters.OPERATION_PUBLISHER));

        // REL_POL_US_AC
        target.setRelPolUsAc(internationalStringToEntity(source.getRelPolUsAc(), target.getRelPolUsAc(), ServiceExceptionParameters.OPERATION_REL_POL_US_AC));

        // RELEASE_CALENDAR
        target.setReleaseCalendar(source.getReleaseCalendar());

        // RELEASE_CALENDAR_ACCESS
        target.setReleaseCalendarAccess(source.getReleaseCalendarAccess());

        // UPDATE_FREQUENCY
        target.getUpdateFrequency().clear();
        target.getUpdateFrequency().addAll(externalItemListToEntity(source.getUpdateFrequency(), target.getUpdateFrequency(), ServiceExceptionParameters.OPERATION_UPDATE_FREQUENCY));

        // CURRENT_INTERNAL_INSTANCE
        // Not necessary. Extracted from instances order.

        // CURRENT_INSTANCE.
        // Not necessary. Extracted from instances order.

        // INVENTORY_DATE
        // Not necessary. It can't be manually modified.

        // REV_POLICY
        target.setRevPolicy(internationalStringToEntity(source.getRevPolicy(), target.getRevPolicy(), ServiceExceptionParameters.OPERATION_REV_POLICY));

        // REV_PRACTICE
        target.setRevPractice(internationalStringToEntity(source.getRevPractice(), target.getRevPractice(), ServiceExceptionParameters.OPERATION_REV_PRACTICE));

        // CONTACT: Extracted from AppCommonMetadata

        // LEGAL_ACTS: Extracted from AppCommonMetadata
        target.setSpecificLegalActs(internationalStringToEntity(source.getSpecificLegalActs(), target.getSpecificLegalActs(), ServiceExceptionParameters.OPERATION_SPECIFIC_LEGAL_ACTS));
        // DATA_SHARING: Extracted from AppCommonMetadata
        target.setSpecificDataSharing(internationalStringToEntity(source.getSpecificDataSharing(), target.getSpecificDataSharing(), ServiceExceptionParameters.OPERATION_SPECIFIC_DATA_SHARING));
        // CONFIDENCIALITY_POLICY: Extracted from AppCommonMetadata
        // CONFIDENCIALITY_DATA_TREATMENT: Extracted from AppCommonMetadata

        // COMMENT
        target.setComment(internationalStringToEntity(source.getComment(), target.getComment(), ServiceExceptionParameters.OPERATION_COMMENT));

        // NOTES
        target.setNotes(internationalStringToEntity(source.getNotes(), target.getNotes(), ServiceExceptionParameters.OPERATION_NOTES));

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

        // URN
        // Not necessary. It can't be manually modified

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
        target.getStatisticalUnit().addAll(externalItemListToEntity(source.getStatisticalUnit(), target.getStatisticalUnit(), ServiceExceptionParameters.INSTANCE_STATISTICAL_UNIT));

        // GEOGRAPHIC_GRANULARITY
        target.getGeographicGranularity().clear();
        target.getGeographicGranularity().addAll(externalItemListToEntity(source.getGeographicGranularity(), target.getGeographicGranularity(), ServiceExceptionParameters.INSTANCE_GEOGRAPHIC_GRANULARITY));

        // GEOGRAPHIC_COMPARABILITY
        target.setGeographicComparability(internationalStringToEntity(source.getGeographicComparability(), target.getGeographicComparability(),
                ServiceExceptionParameters.INSTANCE_GEOGRAPHIC_COMPARABILITY));

        // TEMPORAL_GRANULARITY
        target.getTemporalGranularity().clear();
        target.getTemporalGranularity().addAll(externalItemListToEntity(source.getTemporalGranularity(), target.getTemporalGranularity(), ServiceExceptionParameters.INSTANCE_TEMPORAL_GRANULARITY));

        // TEMPORAL_COMPARABILITY
        target.setTemporalComparability(internationalStringToEntity(source.getTemporalComparability(), target.getTemporalComparability(), ServiceExceptionParameters.INSTANCE_TEMPORAL_COMPARABILITY));

        // BASE_PERIOD
        target.setBasePeriod(source.getBasePeriod());

        // UNIT_MEASURE
        target.getUnitMeasure().clear();
        target.getUnitMeasure().addAll(externalItemListToEntity(source.getUnitMeasure(), target.getUnitMeasure(), ServiceExceptionParameters.INSTANCE_UNIT_MEASURE));

        // STAT_CONC_DEF
        target.setStatConcDef(internationalStringToEntity(source.getStatConcDef(), target.getStatConcDef(), ServiceExceptionParameters.INSTANCE_STAT_CONC_DEF));

        // STAT_CONC_DEF_LIST
        target.getStatConcDefList().clear();
        target.getStatConcDefList().addAll(externalItemListToEntity(source.getStatConcDefList(), target.getStatConcDefList(), ServiceExceptionParameters.INSTANCE_STAT_CONC_DEF_LIST));

        // CLASS_SYSTEM
        target.setClassSystem(internationalStringToEntity(source.getClassSystem(), target.getClassSystem(), ServiceExceptionParameters.INSTANCE_CLASS_SYSTEM));

        // CLASS_SYSTEM_LIST
        target.getClassSystemList().clear();
        target.getClassSystemList().addAll(externalItemListToEntity(source.getClassSystemList(), target.getClassSystemList(), ServiceExceptionParameters.INSTANCE_CLASS_SYSTEM_LIST));

        // INSTANCE_TYPE
        target.setInstanceType(instanceTypeDtoToEntity(source.getInstanceType(), ctx));

        // INTERNAL_INVENTORY_DATE
        // Not necessary. It can't be manually edited.

        // PROC_STATUS
        // Not necessary. It can't be manually edited.

        // DOC_METHOD
        target.setDocMethod(internationalStringToEntity(source.getDocMethod(), target.getDocMethod(), ServiceExceptionParameters.INSTANCE_DOC_METHOD));

        // SURVEY_SOURCE
        target.setSurveySource(surveySourceDtoToEntity(source.getSurveySource(), ctx));

        // COLL_METHOD
        target.setCollMethod(collMethodDtoToEntity(source.getCollMethod(), ctx));

        // INFORMATION_SUPPLIERS
        target.getInformationSuppliers().clear();
        target.getInformationSuppliers()
                .addAll(externalItemListToEntity(source.getInformationSuppliers(), target.getInformationSuppliers(), ServiceExceptionParameters.INSTANCE_INFORMATION_SUPPLIERS));

        // FREQ_COLL
        target.getFreqColl().clear();
        target.getFreqColl().addAll(externalItemListToEntity(source.getFreqColl(), target.getFreqColl(), ServiceExceptionParameters.INSTANCE_FREQ_COLL));

        // DATA_VALIDATION
        target.setDataValidation(internationalStringToEntity(source.getDataValidation(), target.getDataValidation(), ServiceExceptionParameters.INSTANCE_DATA_VALIDATION));

        // DATA_COMPILATION
        target.setDataCompilation(internationalStringToEntity(source.getDataCompilation(), target.getDataCompilation(), ServiceExceptionParameters.INSTANCE_DATA_COMPILATION));

        // ADJUSTMENT
        target.setAdjustment(internationalStringToEntity(source.getAdjustment(), target.getAdjustment(), ServiceExceptionParameters.INSTANCE_ADJUSTMENT));

        // COST_BURDEN
        target.setCostBurden(internationalStringToEntity(source.getCostBurden(), target.getCostBurden(), ServiceExceptionParameters.INSTANCE_COST_BURDEN));

        // COST
        target.getCost().addAll(costDtoListToCostList(source.getCost(), target.getCost(), ctx));

        // INVENTORY_DATE
        // Not necessary. It can't be manually edited.

        // QUALITY_DOC
        target.setQualityDoc(internationalStringToEntity(source.getQualityDoc(), target.getQualityDoc(), ServiceExceptionParameters.INSTANCE_QUALITY_DOC));

        // QUALITY_ASSURE
        target.setQualityAssure(internationalStringToEntity(source.getQualityAssure(), target.getQualityAssure(), ServiceExceptionParameters.INSTANCE_QUALITY_ASSURE));

        // QUALITY_ASSMNT
        target.setQualityAssmnt(internationalStringToEntity(source.getQualityAssmnt(), target.getQualityAssmnt(), ServiceExceptionParameters.INSTANCE_QUALITY_ASSMNT));

        // USER_NEEDS
        target.setUserNeeds(internationalStringToEntity(source.getUserNeeds(), target.getUserNeeds(), ServiceExceptionParameters.INSTANCE_USER_NEEDS));

        // USER_SAT
        target.setUserSat(internationalStringToEntity(source.getUserSat(), target.getUserSat(), ServiceExceptionParameters.INSTANCE_USER_SAT));

        // COMPLETENESS
        target.setCompleteness(internationalStringToEntity(source.getCompleteness(), target.getCompleteness(), ServiceExceptionParameters.INSTANCE_COMPLETENESS));

        // TIMELINESS
        target.setTimeliness(internationalStringToEntity(source.getTimeliness(), target.getTimeliness(), ServiceExceptionParameters.INSTANCE_TIMELINESS));

        // PUNCTUALITY
        target.setPunctuality(internationalStringToEntity(source.getPunctuality(), target.getPunctuality(), ServiceExceptionParameters.INSTANCE_PUNCTUALITY));

        // ACCURACY_OVERALL
        target.setAccuracyOverall(internationalStringToEntity(source.getAccuracyOverall(), target.getAccuracyOverall(), ServiceExceptionParameters.INSTANCE_ACCURACY_OVERALL));

        // SAMPLING_ERR
        target.setSamplingErr(internationalStringToEntity(source.getSamplingErr(), target.getSamplingErr(), ServiceExceptionParameters.INSTANCE_SAMPLING_ERR));

        // NONSAMPLING_ERR
        target.setNonsamplingErr(internationalStringToEntity(source.getNonsamplingErr(), target.getNonsamplingErr(), ServiceExceptionParameters.INSTANCE_NONSAMPLING_ERR));

        // COHER_X_DOMAIN
        target.setCoherXDomain(internationalStringToEntity(source.getCoherXDomain(), target.getCoherXDomain(), ServiceExceptionParameters.INSTANCE_COHER_X_DOMAIN));

        // COHER_INTERNAL
        target.setCoherInternal(internationalStringToEntity(source.getCoherInternal(), target.getCoherInternal(), ServiceExceptionParameters.INSTANCE_COHER_INTERNAL));

        // COMMENT
        target.setComment(internationalStringToEntity(source.getComment(), target.getComment(), ServiceExceptionParameters.INSTANCE_COMMENT));

        // NOTES
        target.setNotes(internationalStringToEntity(source.getNotes(), target.getNotes(), ServiceExceptionParameters.INSTANCE_NOTES));

        // Optimistic locking: Update "update date" attribute to force update of the root entity in order to increase attribute "version"
        target.setUpdateDate(new DateTime());

        return target; 
    }

    // ------------------------------------------------------------
    // EXTERNAL ITEMS
    // ------------------------------------------------------------
    
    private Set<ExternalItem> externalItemListToEntity(Set<ExternalItemDto> sources, Set<ExternalItem> targets, String metadataName) throws MetamacException {

        Set<ExternalItem> targetsBefore = targets;
        Set<ExternalItem> newTargets = new HashSet<ExternalItem>();

        for (ExternalItemDto source : sources) {
            boolean existsBefore = false;
            for (ExternalItem target : targetsBefore) {
                if (source.getUrn().equals(target.getUrn())) {
                    newTargets.add(externalItemDtoToEntity(source, target, metadataName));
                    existsBefore = true;
                    break;
                }
            }
            if (!existsBefore) {
                newTargets.add(externalItemDtoToEntity(source, null, metadataName));
            }
        }

        // Delete missing
        for (ExternalItem oldTarget : targetsBefore) {
            boolean found = false;
            for (ExternalItem newTarget : newTargets) {
                found = found || (oldTarget.getUrn().equals(newTarget.getUrn()));
            }
            if (!found) {
                // Delete
                externalItemDtoToEntity(null, oldTarget, metadataName);
            }
        }

        targets.clear();
        for (ExternalItem target : newTargets) {
            targets.add(target);
        }

        return targets;
    }

    private ExternalItem externalItemDtoToEntity(ExternalItemDto source, ExternalItem target, String metadataName) throws MetamacException {
        if (source == null) {
            if (target != null) {
                // delete previous entity
                externalItemRepository.delete(target);
            }
            return null;
        }

        if (target == null) {
            target = new ExternalItem(source.getCode(), source.getUri(), source.getUrn(), source.getType(), internationalStringToEntity(source.getTitle(), null, metadataName),
                    source.getManagementAppUrl());
        } else {
            target.setCode(source.getCode());
            target.setUri(source.getUri());
            target.setUrn(source.getUrn());
            target.setType(source.getType());
            target.setManagementAppUrl(source.getManagementAppUrl());
            target.setTitle(internationalStringToEntity(source.getTitle(), target.getTitle(), metadataName));
        }

        return target;
    }


    // ------------------------------------------------------------
    // INTERNATIONAL STRINGS & LOCALISED STRINGS
    // ------------------------------------------------------------
    
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

        Set<LocalisedString> localisedStringEntities = localisedStringDtoToDo(source.getTexts(), target.getTexts(), target);
        target.getTexts().clear();
        target.getTexts().addAll(localisedStringEntities);

        return target;
    }

    private Set<LocalisedString> localisedStringDtoToDo(Set<LocalisedStringDto> sources, Set<LocalisedString> targets, InternationalString internationalStringTarget) {

        Set<LocalisedString> targetsBefore = targets;
        targets = new HashSet<LocalisedString>();

        for (LocalisedStringDto source : sources) {
            boolean existsBefore = false;
            for (LocalisedString target : targetsBefore) {
                if (source.getLocale().equals(target.getLocale())) {
                    targets.add(localisedStringDtoToDo(source, target, internationalStringTarget));
                    existsBefore = true;
                    break;
                }
            }
            if (!existsBefore) {
                targets.add(localisedStringDtoToDo(source, internationalStringTarget));
            }
        }
        return targets;
    }

    private LocalisedString localisedStringDtoToDo(LocalisedStringDto source, InternationalString internationalStringTarget) {
        LocalisedString target = new LocalisedString();
        localisedStringDtoToDo(source, target, internationalStringTarget);
        return target;
    }

    private LocalisedString localisedStringDtoToDo(LocalisedStringDto source, LocalisedString target, InternationalString internationalStringTarget) {
        target.setLabel(source.getLabel());
        target.setLocale(source.getLocale());
        target.setIsUnmodifiable(source.getIsUnmodifiable());
        target.setInternationalString(internationalStringTarget);
        return target;
    }

    
    // ------------------------------------------------------------
    // COSTS
    // ------------------------------------------------------------
    
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
