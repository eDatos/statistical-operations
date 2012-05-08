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
    public SurveyTypeDto surveyTypeToDto(SurveyType source) {
        if (source == null) {
            return null;
        }

        SurveyTypeDto target = getMapper().map(source, SurveyTypeDto.class);

        // DESCRIPTION
        target.setDescription(internationalStringToDto(source.getDescription()));

        return target;
    }

    @Override
    public InstanceTypeDto instanceTypeToDto(InstanceType source) {
        if (source == null) {
            return null;
        }

        InstanceTypeDto target = getMapper().map(source, InstanceTypeDto.class);

        // DESCRIPTION
        target.setDescription(internationalStringToDto(source.getDescription()));

        return target;
    }

    @Override
    public SurveySourceDto surveySourceToDto(SurveySource source) {
        if (source == null) {
            return null;
        }

        SurveySourceDto target = getMapper().map(source, SurveySourceDto.class);

        // DESCRIPTION
        target.setDescription(internationalStringToDto(source.getDescription()));

        return target;
    }

    @Override
    public OfficialityTypeDto officialityTypeToDto(OfficialityType source) {
        if (source == null) {
            return null;
        }

        OfficialityTypeDto target = getMapper().map(source, OfficialityTypeDto.class);

        // DESCRIPTION
        target.setDescription(internationalStringToDto(source.getDescription()));

        return target;
    }

    @Override
    public CollMethodDto collMethodToDto(CollMethod source) {
        if (source == null) {
            return null;
        }

        CollMethodDto target = getMapper().map(source, CollMethodDto.class);

        // DESCRIPTION
        target.setDescription(internationalStringToDto(source.getDescription()));

        return target;
    }

    @Override
    public CostDto costToDto(Cost source) {
        if (source == null) {
            return null;
        }

        CostDto target = getMapper().map(source, CostDto.class);

        // DESCRIPTION
        target.setDescription(internationalStringToDto(source.getDescription()));

        return target;
    }

    /**************************************************************************
     * PUBLIC - ENTITIES
     **************************************************************************/

    public FamilyDto familyToDto(Family source) {
        if (source == null) {
            return null;
        }

        FamilyDto target = getMapper().map(source, FamilyDto.class);

        // CODE
        // Not necessary

        // TITLE
        target.setTitle(internationalStringToDto(source.getTitle()));

        // TITLE_ALTERNATIVE
        target.setAcronym(internationalStringToDto(source.getAcronym()));

        // DESCRIPTION
        target.setDescription(internationalStringToDto(source.getDescription()));

        // INTERNAL_INVENTORY_DATE
        // Not necessary

        // PROC STATUS
        // Not necessary

        // INVENTORY_DATE
        // Not necessary
        
        target.setOptimisticLockingVersion(source.getVersion());

        return target;
    }

    public FamilyBaseDto familyToBaseDto(Family source) {
        if (source == null) {
            return null;
        }

        FamilyBaseDto target = getMapper().map(source, FamilyBaseDto.class);

        // IDENTIFIER
        // Not necessary

        // TITLE
        target.setTitle(internationalStringToDto(source.getTitle()));

        // DESCRIPTION
        target.setDescription(internationalStringToDto(source.getDescription()));

        // PROC STATUS
        // Not necessary

        return target;
    }

    public OperationDto operationToDto(Operation source) {
        if (source == null) {
            return null;
        }

        OperationDto target = getMapper().map(source, OperationDto.class);

        // APP_COMMON_METADATA
        target.setCommonMetadata(externalItemBtToDto(source.getCommonMetadata()));

        // CODE
        // Not necessary

        // TITLE
        target.setTitle(internationalStringToDto(source.getTitle()));

        // TITLE_ALETERNATIVE
        target.setAcronym(internationalStringToDto(source.getAcronym()));

        // SUBJECT_AREA
        target.setSubjectArea(externalItemBtToDto(source.getSubjectArea()));

        // SECONDARY_SUBJECT_AREAS
        target.getSecondarySubjectAreas().addAll(externalItemListToDto(source.getSecondarySubjectAreas()));

        // OBJECTIVE
        target.setObjective(internationalStringToDto(source.getObjective()));

        // DESCRIPTION
        target.setDescription(internationalStringToDto(source.getDescription()));

        // SURVEY_TYPE
        target.setSurveyType(surveyTypeToDto(source.getSurveyType()));

        // OFFICIALITY_TYPE
        target.setOfficialityType(officialityTypeToDto(source.getOfficialityType()));

        // INDICATOR_SYSTEM
        // Not necessary

        // PRODUCER
        target.getProducer().addAll(externalItemListToDto(source.getProducer()));

        // REGIONAL_RESPONSIBLE
        target.getRegionalResponsible().addAll(externalItemListToDto(source.getRegionalResponsible()));

        // REGIONAL_CONTRIBUTOR
        target.getRegionalContributor().addAll(externalItemListToDto(source.getRegionalContributor()));

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
        target.getPublisher().addAll(externalItemListToDto(source.getPublisher()));

        // REL_POL_US_AC
        target.setRelPolUsAc(internationalStringToDto(source.getRelPolUsAc()));
        // REL_POL_US_AC_URL
        // Not necessary

        // RELEASE_CALENDAR
        // Not necessary
        // RELEASE_CALENDAR_ACCESS
        // Not necessary

        // UPDATE_FREQUENCY
        target.getUpdateFrequency().addAll(externalItemListToDto(source.getUpdateFrequency()));

        // CURRENT_INSTANCE
        // Transformed in service

        // INVENTORY_DATE
        // Not necessary

        // REV_POLICY
        target.setRevPolicy(internationalStringToDto(source.getRevPolicy()));
        // REV_POLICY_URL
        // Not necessary

        // REV_PRACTICE
        target.setRevPractice(internationalStringToDto(source.getRevPractice()));
        // REV_PRACTICE_URL
        // Not necessary

        // CONTACT: Extracted from AppCommonMetadata
        // LEGAL_ACTS: Extracted from AppCommonMetadata
        // DATA_SHARING: Extracted from AppCommonMetadata
        // CONFIDENCIALITY_POLICY: Extracted from AppCommonMetadata
        // CONFIDENCIALITY_DATA_TREATMENT: Extracted from AppCommonMetadata

        // COMMENT
        target.setComment(internationalStringToDto(source.getComment()));
        // COMMENT_URL
        // Not necessary

        // NOTES
        target.setNotes(internationalStringToDto(source.getNotes()));
        // NOTES_URL
        // Not necessary
        
        target.setOptimisticLockingVersion(source.getVersion());

        return target;
    }

    public OperationBaseDto operationToBaseDto(Operation source) {
        if (source == null) {
            return null;
        }

        OperationBaseDto target = getMapper().map(source, OperationBaseDto.class);

        // CODE
        // Not necessary

        // TITLE
        target.setTitle(internationalStringToDto(source.getTitle()));

        // TITLE_ALTERNATIVE
        target.setAcronym(internationalStringToDto(source.getAcronym()));

        // PROC_STATUS
        // Not necessary

        return target;
    }

    @Override
    public InstanceDto instanceToDto(Instance source) {
        if (source == null) {
            return null;
        }

        InstanceDto target = getMapper().map(source, InstanceDto.class);

        // ORDER
        // Not necessary

        // CODE
        // Not necessary

        // TITLE
        target.setTitle(internationalStringToDto(source.getTitle()));

        // TITLE_ALETERNATIVE
        target.setAcronym(internationalStringToDto(source.getAcronym()));

        // SURVEY_CODE
        // Instance DTO doesn't have Operation Information.

        // DATA_DESCRIPTION
        target.setDataDescription(internationalStringToDto(source.getDataDescription()));

        // STATISTICAL_POPULATION
        target.setStatisticalPopulation(internationalStringToDto(source.getStatisticalPopulation()));

        // STATISTICAL_UNIT
        target.getStatisticalUnit().addAll(externalItemListToDto(source.getStatisticalUnit()));

        // GEOGRAPHIC_GRANULARITY
        target.setGeographicGranularity(externalItemBtToDto(source.getGeographicGranularity()));

        // GEOGRAPHIC_COMPARABILITY
        target.setGeographicComparability(internationalStringToDto(source.getGeographicComparability()));

        // TEMPORAL_GRANULARITY
        target.setTemporalGranularity(externalItemBtToDto(source.getTemporalGranularity()));

        // TEMPORAL_COMPARABILITY
        target.setTemporalComparability(internationalStringToDto(source.getTemporalComparability()));

        // BASE_PERIOD
        // Not necessary

        // UNIT_MEASURE
        target.getUnitMeasure().addAll(externalItemListToDto(source.getUnitMeasure()));

        // STAT_CONC_DEF
        target.setStatConcDef(internationalStringToDto(source.getStatConcDef()));

        // STAT_CONC_DEF_LIST
        target.getStatConcDefList().addAll(externalItemListToDto(source.getStatConcDefList()));

        // CLASS_SYSTEM
        target.setClassSystem(internationalStringToDto(source.getClassSystem()));

        // CLASS_SYSTEM_LIST
        target.getClassSystemList().addAll(externalItemListToDto(source.getClassSystemList()));

        // INSTANCE_TYPE
        target.setInstanceType(instanceTypeToDto(source.getInstanceType()));

        // INTERNAL_INVENTORY_DATE
        // Not necessary

        // INACTIVE_DATE
        // Not necessary

        // PROC_STATUS
        // Not necessary

        // DOC_METHOD
        target.setDocMethod(internationalStringToDto(source.getDocMethod()));

        // DOC_METHOD_URL
        // Not necessary

        // SURVEY_SOURCE
        target.setSurveySource(surveySourceToDto(source.getSurveySource()));

        // COLL_METHOD
        target.setCollMethod(collMethodToDto(source.getCollMethod()));

        // INFORMATION_SUPPLIERS
        target.getInformationSuppliers().addAll(externalItemListToDto(source.getInformationSuppliers()));

        // FREQ_COLL
        target.getFreqColl().addAll(externalItemListToDto(source.getFreqColl()));

        // DATA_VALIDATION
        target.setDataValidation(internationalStringToDto(source.getDataValidation()));
        // DATA_VALIDATION_URL
        // Not necessary

        // DATA_COMPILATION
        target.setDataCompilation(internationalStringToDto(source.getDataCompilation()));
        // DATA_COMPILATION_URL
        // Not necessary

        // ADJUSTMENT
        target.setAdjustment(internationalStringToDto(source.getAdjustment()));
        // ADJUSTMENT_URL
        // Not necessary

        // COST_BURDEN
        target.setCostBurden(internationalStringToDto(source.getCostBurden()));
        // COST_BURDEN_URL
        // Not necessary

        // COST
        target.getCost().addAll(costListToDto(source.getCost()));

        // INVENTORY_DATE
        // Not necessary

        // QUALITY_DOC
        target.setQualityDoc(internationalStringToDto(source.getQualityDoc()));
        // QUALITY_DOC_URL
        // Not necessary

        // QUALITY_ASSURE
        target.setQualityAssure(internationalStringToDto(source.getQualityAssure()));
        // QUALITY_ASSURE_URL
        // Not necessary

        // QUALITY_ASSMNT
        target.setQualityAssmnt(internationalStringToDto(source.getQualityAssmnt()));
        // QUALITY_ASSMNT_URL
        // Not necessary

        // USER_NEEDS
        target.setUserNeeds(internationalStringToDto(source.getUserNeeds()));
        // USER_NEEDS_URL
        // Not necessary

        // USER_SAT
        target.setUserSat(internationalStringToDto(source.getUserSat()));
        // USER_SAT_URL
        // Not necessary

        // COMPLETENESS
        target.setCompleteness(internationalStringToDto(source.getCompleteness()));
        // COMPLETENESS_URL
        // Not necessary

        // TIMELINESS
        target.setTimeliness(internationalStringToDto(source.getTimeliness()));
        // TIMELINESS_URL
        // Not necessary

        // PUNCTUALITY
        target.setPunctuality(internationalStringToDto(source.getPunctuality()));
        // PUNCTUALITY_URL
        // Not necessary

        // ACCURACY_OVERALL
        target.setAccuracyOverall(internationalStringToDto(source.getAccuracyOverall()));
        // ACCURACY_OVERALL_URL
        // Not necessary

        // SAMPLING_ERR
        target.setSamplingErr(internationalStringToDto(source.getSamplingErr()));
        // SAMPLING_ERR_URL
        // Not necessary

        // NONSAMPLING_ERR
        target.setNonsamplingErr(internationalStringToDto(source.getNonsamplingErr()));
        // NONSAMPLING_ERR_URL
        // Not necessary

        // COHER_X_DOMAIN
        target.setCoherXDomain(internationalStringToDto(source.getCoherXDomain()));
        // COHER_X_DOMAIN_URL
        // Not necessary

        // COHER_INTERNAL
        target.setCoherInternal(internationalStringToDto(source.getCoherInternal()));
        // COHER_INTERNAL_URL
        // Not necessary

        // COMMENT
        target.setComment(internationalStringToDto(source.getComment()));
        // COMMENT_URL
        // Not necessary

        // NOTES
        target.setNotes(internationalStringToDto(source.getNotes()));
        // NOTES_URL
        // Not necessary

        target.setOptimisticLockingVersion(source.getVersion());
        
        return target;
    }

    @Override
    public InstanceBaseDto instanceToBaseDto(Instance source) {
        if (source == null) {
            return null;
        }

        InstanceBaseDto target = getMapper().map(source, InstanceBaseDto.class);

        // CODE
        // Not necessary

        // TITLE
        target.setTitle(internationalStringToDto(source.getTitle()));

        // PROC_STATUS
        // Not necessary

        // ORDER
        // Not necessary

        return target;
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
