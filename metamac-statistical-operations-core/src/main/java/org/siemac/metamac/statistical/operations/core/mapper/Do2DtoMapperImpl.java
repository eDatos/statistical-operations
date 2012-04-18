package org.siemac.metamac.statistical.operations.core.mapper;

import java.util.HashSet;
import java.util.Set;

import org.dozer.DozerBeanMapper;
import org.siemac.metamac.core.common.bt.domain.ExternalItemBt;
import org.siemac.metamac.core.common.dto.ExternalItemBtDto;
import org.siemac.metamac.core.common.dto.InternationalStringDto;
import org.siemac.metamac.core.common.dto.LocalisedStringDto;
import org.siemac.metamac.core.common.ent.domain.InternationalString;
import org.siemac.metamac.core.common.ent.domain.LocalisedString;
import org.siemac.metamac.core.common.vo.domain.ExternalItem;
import org.siemac.metamac.domain.statistical.operations.dto.CollMethodDto;
import org.siemac.metamac.domain.statistical.operations.dto.CostDto;
import org.siemac.metamac.domain.statistical.operations.dto.FamilyBaseDto;
import org.siemac.metamac.domain.statistical.operations.dto.FamilyDto;
import org.siemac.metamac.domain.statistical.operations.dto.InstanceBaseDto;
import org.siemac.metamac.domain.statistical.operations.dto.InstanceDto;
import org.siemac.metamac.domain.statistical.operations.dto.InstanceTypeDto;
import org.siemac.metamac.domain.statistical.operations.dto.OfficialityTypeDto;
import org.siemac.metamac.domain.statistical.operations.dto.OperationBaseDto;
import org.siemac.metamac.domain.statistical.operations.dto.OperationDto;
import org.siemac.metamac.domain.statistical.operations.dto.SurveySourceDto;
import org.siemac.metamac.domain.statistical.operations.dto.SurveyTypeDto;
import org.siemac.metamac.statistical.operations.core.domain.CollMethod;
import org.siemac.metamac.statistical.operations.core.domain.Cost;
import org.siemac.metamac.statistical.operations.core.domain.Family;
import org.siemac.metamac.statistical.operations.core.domain.Instance;
import org.siemac.metamac.statistical.operations.core.domain.InstanceType;
import org.siemac.metamac.statistical.operations.core.domain.OfficialityType;
import org.siemac.metamac.statistical.operations.core.domain.Operation;
import org.siemac.metamac.statistical.operations.core.domain.SurveySource;
import org.siemac.metamac.statistical.operations.core.domain.SurveyType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class Do2DtoMapperImpl implements Do2DtoMapper {

    @Autowired
    private DozerBeanMapper mapper;

    /**************************************************************************
     * GETTERS
     **************************************************************************/
    protected DozerBeanMapper getMapper() {
        return mapper;
    }

    /**************************************************************************
     * PUBLIC - LISTS
     **************************************************************************/

    @Override
    public SurveyTypeDto surveyTypeToDto(SurveyType surveyType) {
        if (surveyType == null) {
            return null;
        }

        SurveyTypeDto surveyTypeDto = getMapper().map(surveyType, SurveyTypeDto.class);

        // DESCRIPTION
        surveyTypeDto.setDescription(internationalStringToDto(surveyType.getDescription()));

        return surveyTypeDto;
    }

    @Override
    public InstanceTypeDto instanceTypeToDto(InstanceType instanceType) {
        if (instanceType == null) {
            return null;
        }

        InstanceTypeDto instanceTypeDto = getMapper().map(instanceType, InstanceTypeDto.class);

        // DESCRIPTION
        instanceTypeDto.setDescription(internationalStringToDto(instanceType.getDescription()));

        return instanceTypeDto;
    }

    @Override
    public SurveySourceDto surveySourceToDto(SurveySource surveySource) {
        if (surveySource == null) {
            return null;
        }

        SurveySourceDto surveySourceDto = getMapper().map(surveySource, SurveySourceDto.class);

        // DESCRIPTION
        surveySourceDto.setDescription(internationalStringToDto(surveySource.getDescription()));

        return surveySourceDto;
    }

    @Override
    public OfficialityTypeDto officialityTypeToDto(OfficialityType officialityType) {
        if (officialityType == null) {
            return null;
        }

        OfficialityTypeDto officialityTypeDto = getMapper().map(officialityType, OfficialityTypeDto.class);

        // DESCRIPTION
        officialityTypeDto.setDescription(internationalStringToDto(officialityType.getDescription()));

        return officialityTypeDto;
    }

    @Override
    public CollMethodDto collMethodToDto(CollMethod collMethod) {
        if (collMethod == null) {
            return null;
        }

        CollMethodDto collMethodDto = getMapper().map(collMethod, CollMethodDto.class);

        // DESCRIPTION
        collMethodDto.setDescription(internationalStringToDto(collMethod.getDescription()));

        return collMethodDto;
    }

    @Override
    public CostDto costToDto(Cost cost) {
        if (cost == null) {
            return null;
        }

        CostDto costDto = getMapper().map(cost, CostDto.class);

        // DESCRIPTION
        costDto.setDescription(internationalStringToDto(cost.getDescription()));

        return costDto;
    }

    /**************************************************************************
     * PUBLIC - ENTITIES
     **************************************************************************/

    public FamilyDto familyToDto(Family family) {
        if (family == null) {
            return null;
        }

        FamilyDto familyDto = getMapper().map(family, FamilyDto.class);

        // CODE
        // Not necessary

        // TITLE
        familyDto.setTitle(internationalStringToDto(family.getTitle()));

        // TITLE_ALTERNATIVE
        familyDto.setAcronym(internationalStringToDto(family.getAcronym()));

        // DESCRIPTION
        familyDto.setDescription(internationalStringToDto(family.getDescription()));

        // INTERNAL_INVENTORY_DATE
        // Not necessary

        // PROC STATUS
        // Not necessary

        // INVENTORY_DATE
        // Not necessary

        return familyDto;
    }

    public FamilyBaseDto familyToBaseDto(Family family) {
        if (family == null) {
            return null;
        }

        FamilyBaseDto familyBaseDto = getMapper().map(family, FamilyBaseDto.class);

        // IDENTIFIER
        // Not necessary

        // TITLE
        familyBaseDto.setTitle(internationalStringToDto(family.getTitle()));

        // DESCRIPTION
        familyBaseDto.setDescription(internationalStringToDto(family.getDescription()));

        // PROC STATUS
        // Not necessary

        return familyBaseDto;
    }

    public OperationDto operationToDto(Operation operation) {
        if (operation == null) {
            return null;
        }

        OperationDto operationDto = getMapper().map(operation, OperationDto.class);

        // APP_COMMON_METADATA
        operationDto.setCommonMetadata(externalItemBtToDto(operation.getCommonMetadata()));

        // CODE
        // Not necessary

        // TITLE
        operationDto.setTitle(internationalStringToDto(operation.getTitle()));

        // TITLE_ALETERNATIVE
        operationDto.setAcronym(internationalStringToDto(operation.getAcronym()));

        // SUBJECT_AREA
        operationDto.setSubjectArea(externalItemBtToDto(operation.getSubjectArea()));

        // SECONDARY_SUBJECT_AREAS
        operationDto.getSecondarySubjectAreas().addAll(externalItemListToDto(operation.getSecondarySubjectAreas()));

        // OBJECTIVE
        operationDto.setObjective(internationalStringToDto(operation.getObjective()));

        // DESCRIPTION
        operationDto.setDescription(internationalStringToDto(operation.getDescription()));

        // SURVEY_TYPE
        operationDto.setSurveyType(surveyTypeToDto(operation.getSurveyType()));

        // OFFICIALITY_TYPE
        operationDto.setOfficialityType(officialityTypeToDto(operation.getOfficialityType()));

        // INDICATOR_SYSTEM
        // Not necessary

        // PRODUCER
        operationDto.getProducer().addAll(externalItemListToDto(operation.getProducer()));

        // REGIONAL_RESPONSIBLE
        operationDto.getRegionalResponsible().addAll(externalItemListToDto(operation.getRegionalResponsible()));

        // REGIONAL_CONTRIBUTOR
        operationDto.getRegionalContributor().addAll(externalItemListToDto(operation.getRegionalContributor()));

        // INTERNAL_INVENTORY_DATE
        // Not necessary

        // CURRENTLY_ACTIVE
        // Not necessary

        // INACTIVE_DATE
        // Not necessary

        // PROC_STATUS
        // Not necessary

        // CURRENT_INTERNAL_INSTANCE
        // Transformed in service

        // PUBLISHER
        operationDto.getPublisher().addAll(externalItemListToDto(operation.getPublisher()));

        // REL_POL_US_AC
        operationDto.setRelPolUsAc(internationalStringToDto(operation.getRelPolUsAc()));
        // REL_POL_US_AC_URL
        // Not necessary

        // RELEASE_CALENDAR
        // Not necessary
        // RELEASE_CALENDAR_ACCESS
        // Not necessary

        // UPDATE_FREQUENCY
        operationDto.getUpdateFrequency().addAll(externalItemListToDto(operation.getUpdateFrequency()));

        // CURRENT_INSTANCE
        // Transformed in service

        // INVENTORY_DATE
        // Not necessary

        // REV_POLICY
        operationDto.setRevPolicy(internationalStringToDto(operation.getRevPolicy()));
        // REV_POLICY_URL
        // Not necessary

        // REV_PRACTICE
        operationDto.setRevPractice(internationalStringToDto(operation.getRevPractice()));
        // REV_PRACTICE_URL
        // Not necessary

        // CONTACT: Extracted from AppCommonMetadata
        // TODO

        // LEGAL_ACTS: Extracted from AppCommonMetadata
        // TODO
        // DATA_SHARING: Extracted from AppCommonMetadata
        // TODO
        // CONFIDENCIALITY_POLICY: Extracted from AppCommonMetadata
        // TODO
        // CONFIDENCIALITY_DATA_TREATMENT: Extracted from AppCommonMetadata
        // TODO

        // COMMENT
        operationDto.setComment(internationalStringToDto(operation.getComment()));
        // COMMENT_URL
        // Not necessary

        // NOTES
        operationDto.setNotes(internationalStringToDto(operation.getNotes()));
        // NOTES_URL
        // Not necessary

        return operationDto;
    }

    public OperationBaseDto operationToBaseDto(Operation operation) {
        if (operation == null) {
            return null;
        }

        OperationBaseDto operationBaseDto = getMapper().map(operation, OperationBaseDto.class);

        // CODE
        // Not necessary

        // TITLE
        operationBaseDto.setTitle(internationalStringToDto(operation.getTitle()));

        // TITLE_ALTERNATIVE
        operationBaseDto.setAcronym(internationalStringToDto(operation.getAcronym()));

        // PROC_STATUS
        // Not necessary

        return operationBaseDto;
    }

    @Override
    public InstanceDto instanceToDto(Instance instance) {
        if (instance == null) {
            return null;
        }

        InstanceDto instanceDto = getMapper().map(instance, InstanceDto.class);

        // ORDER
        // Not necessary

        // CODE
        // Not necessary

        // TITLE
        instanceDto.setTitle(internationalStringToDto(instance.getTitle()));

        // TITLE_ALETERNATIVE
        instanceDto.setAcronym(internationalStringToDto(instance.getAcronym()));

        // SURVEY_CODE
        // Instance DTO doesn't have Operation Information.

        // DATA_DESCRIPTION
        instanceDto.setDataDescription(internationalStringToDto(instance.getDataDescription()));

        // STATISTICAL_POPULATION
        instanceDto.setStatisticalPopulation(internationalStringToDto(instance.getStatisticalPopulation()));

        // STATISTICAL_UNIT
        instanceDto.getStatisticalUnit().addAll(externalItemListToDto(instance.getStatisticalUnit()));

        // GEOGRAPHIC_GRANULARITY
        instanceDto.setGeographicGranularity(externalItemBtToDto(instance.getGeographicGranularity()));

        // GEOGRAPHIC_COMPARABILITY
        instanceDto.setGeographicComparability(internationalStringToDto(instance.getGeographicComparability()));

        // TEMPORAL_GRANULARITY
        instanceDto.setTemporalGranularity(externalItemBtToDto(instance.getTemporalGranularity()));

        // TEMPORAL_COMPARABILITY
        instanceDto.setTemporalComparability(internationalStringToDto(instance.getTemporalComparability()));

        // BASE_PERIOD
        instanceDto.setBasePeriod(externalItemBtToDto(instance.getBasePeriod()));

        // UNIT_MEASURE
        instanceDto.getUnitMeasure().addAll(externalItemListToDto(instance.getUnitMeasure()));

        // STAT_CONC_DEF
        instanceDto.setStatConcDef(internationalStringToDto(instance.getStatConcDef()));

        // STAT_CONC_DEF_LIST
        instanceDto.getStatConcDefList().addAll(externalItemListToDto(instance.getStatConcDefList()));

        // CLASS_SYSTEM
        instanceDto.setClassSystem(internationalStringToDto(instance.getClassSystem()));

        // CLASS_SYSTEM_LIST
        instanceDto.getClassSystemList().addAll(externalItemListToDto(instance.getClassSystemList()));

        // INSTANCE_TYPE
        instanceDto.setInstanceType(instanceTypeToDto(instance.getInstanceType()));

        // INTERNAL_INVENTORY_DATE
        // Not necessary

        // INACTIVE_DATE
        // Not necessary

        // PROC_STATUS
        // Not necessary

        // DOC_METHOD
        instanceDto.setDocMethod(internationalStringToDto(instance.getDocMethod()));

        // DOC_METHOD_URL
        // Not necessary

        // SURVEY_SOURCE
        instanceDto.setSurveySource(surveySourceToDto(instance.getSurveySource()));

        // COLL_METHOD
        instanceDto.setCollMethod(collMethodToDto(instance.getCollMethod()));

        // INFORMATION_SUPPLIERS
        instanceDto.getInformationSuppliers().addAll(externalItemListToDto(instance.getInformationSuppliers()));

        // FREQ_COLL
        instanceDto.getFreqColl().addAll(externalItemListToDto(instance.getFreqColl()));

        // DATA_VALIDATION
        instanceDto.setDataValidation(internationalStringToDto(instance.getDataValidation()));
        // DATA_VALIDATION_URL
        // Not necessary

        // DATA_COMPILATION
        instanceDto.setDataCompilation(internationalStringToDto(instance.getDataCompilation()));
        // DATA_COMPILATION_URL
        // Not necessary

        // ADJUSTMENT
        instanceDto.setAdjustment(internationalStringToDto(instance.getAdjustment()));
        // ADJUSTMENT_URL
        // Not necessary

        // COST_BURDEN
        instanceDto.setCostBurden(internationalStringToDto(instance.getCostBurden()));
        // COST_BURDEN_URL
        // Not necessary

        // COST
        instanceDto.getCost().addAll(costListToDto(instance.getCost()));

        // INVENTORY_DATE
        // Not necessary

        // QUALITY_DOC
        instanceDto.setQualityDoc(internationalStringToDto(instance.getQualityDoc()));
        // QUALITY_DOC_URL
        // Not necessary

        // QUALITY_ASSURE
        instanceDto.setQualityAssure(internationalStringToDto(instance.getQualityAssure()));
        // QUALITY_ASSURE_URL
        // Not necessary

        // QUALITY_ASSMNT
        instanceDto.setQualityAssmnt(internationalStringToDto(instance.getQualityAssmnt()));
        // QUALITY_ASSMNT_URL
        // Not necessary

        // USER_NEEDS
        instanceDto.setUserNeeds(internationalStringToDto(instance.getUserNeeds()));
        // USER_NEEDS_URL
        // Not necessary

        // USER_SAT
        instanceDto.setUserSat(internationalStringToDto(instance.getUserSat()));
        // USER_SAT_URL
        // Not necessary

        // COMPLETENESS
        instanceDto.setCompleteness(internationalStringToDto(instance.getCompleteness()));
        // COMPLETENESS_URL
        // Not necessary

        // TIMELINESS
        instanceDto.setTimeliness(internationalStringToDto(instance.getTimeliness()));
        // TIMELINESS_URL
        // Not necessary

        // PUNCTUALITY
        instanceDto.setPunctuality(internationalStringToDto(instance.getPunctuality()));
        // PUNCTUALITY_URL
        // Not necessary

        // ACCURACY_OVERALL
        instanceDto.setAccuracyOverall(internationalStringToDto(instance.getAccuracyOverall()));
        // ACCURACY_OVERALL_URL
        // Not necessary

        // SAMPLING_ERR
        instanceDto.setSamplingErr(internationalStringToDto(instance.getSamplingErr()));
        // SAMPLING_ERR_URL
        // Not necessary

        // NONSAMPLING_ERR
        instanceDto.setNonsamplingErr(internationalStringToDto(instance.getNonsamplingErr()));
        // NONSAMPLING_ERR_URL
        // Not necessary

        // COHER_X_DOMAIN
        instanceDto.setCoherXDomain(internationalStringToDto(instance.getCoherXDomain()));
        // COHER_X_DOMAIN_URL
        // Not necessary

        // COHER_INTERNAL
        instanceDto.setCoherInternal(internationalStringToDto(instance.getCoherInternal()));
        // COHER_INTERNAL_URL
        // Not necessary

        // COMMENT
        instanceDto.setComment(internationalStringToDto(instance.getComment()));
        // COMMENT_URL
        // Not necessary

        // NOTES
        instanceDto.setNotes(internationalStringToDto(instance.getNotes()));
        // NOTES_URL
        // Not necessary

        return instanceDto;
    }

    @Override
    public InstanceBaseDto instanceToBaseDto(Instance instance) {
        if (instance == null) {
            return null;
        }

        InstanceBaseDto instanceBaseDto = getMapper().map(instance, InstanceBaseDto.class);

        // CODE
        // Not necessary

        // TITLE
        instanceBaseDto.setTitle(internationalStringToDto(instance.getTitle()));

        // PROC_STATUS
        // Not necessary

        // ORDER
        // Not necessary

        return instanceBaseDto;
    }

    /**************************************************************************
     * PRIVATE
     **************************************************************************/

    /**
     * @param name
     * @return
     */
    private InternationalStringDto internationalStringToDto(InternationalString internationalString) {
        if (internationalString == null) {
            return null;
        }

        // InternationalString to InternationalString Dto
        InternationalStringDto internationalStringDto = getMapper().map(internationalString, InternationalStringDto.class);

        // LocalisedStringDto to LocalisedString
        for (LocalisedString item : internationalString.getTexts()) {
            internationalStringDto.addText(getMapper().map(item, LocalisedStringDto.class));
        }

        return internationalStringDto;
    }

    private Set<ExternalItemBtDto> externalItemListToDto(Set<ExternalItem> statisticalUnit) {

        HashSet<ExternalItemBtDto> result = new HashSet<ExternalItemBtDto>();

        for (ExternalItem externalItem : statisticalUnit) {
            result.add(externalItemToDto(externalItem));
        }

        return result;
    }

    private ExternalItemBtDto externalItemBtToDto(ExternalItemBt externalItem) {
        if (externalItem == null) {
            return null;
        }

        ExternalItemBtDto result = new ExternalItemBtDto(externalItem.getUriInt(), externalItem.getCodeId(), externalItem.getType());

        return result;
    }

    private ExternalItemBtDto externalItemToDto(ExternalItem externalItem) {
        if (externalItem == null) {
            return null;
        }

        ExternalItemBtDto result = new ExternalItemBtDto(externalItem.getExt().getUriInt(), externalItem.getExt().getCodeId(), externalItem.getExt().getType());

        return result;
    }

    private Set<CostDto> costListToDto(Set<Cost> costList) {
        if (costList == null) {
            return null;
        }

        HashSet<CostDto> result = new HashSet<CostDto>();

        for (Cost cost : costList) {
            result.add(costToDto(cost));
        }

        return result;
    }

}
