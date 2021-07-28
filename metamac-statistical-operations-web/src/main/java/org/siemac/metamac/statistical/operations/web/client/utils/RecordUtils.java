package org.siemac.metamac.statistical.operations.web.client.utils;

import static org.siemac.metamac.web.common.client.utils.InternationalStringUtils.getLocalisedString;

import org.siemac.metamac.statistical.operations.core.dto.*;
import org.siemac.metamac.statistical.operations.web.client.model.FamilyRecord;
import org.siemac.metamac.statistical.operations.web.client.model.InstanceRecord;
import org.siemac.metamac.statistical.operations.web.client.model.OperationRecord;
import org.siemac.metamac.web.common.client.utils.CommonWebUtils;
import org.siemac.metamac.web.common.client.utils.DateUtils;

public class RecordUtils {

    /**
     * Returns {@link FamilyRecord} from {@link FamilyBaseDto}
     * 
     * @param familyBaseDto
     * @return
     */
    public static FamilyRecord getFamilyRecord(FamilyBaseDto familyBaseDto) {
        FamilyRecord record = new FamilyRecord();
        record.setId(familyBaseDto.getId());
        record.setUrn(familyBaseDto.getUrn());
        record.setCode(familyBaseDto.getCode());
        record.setTitle(getLocalisedString(familyBaseDto.getTitle()));
        record.setProcStatus(CommonUtils.getProcStatusName(familyBaseDto.getProcStatus()));
        record.setAcronym(getLocalisedString(familyBaseDto.getAcronym()));
        record.setCreatedDate(DateUtils.getFormattedDate(familyBaseDto.getCreatedDate()));
        record.setInternalInventoryDate(DateUtils.getFormattedDate(familyBaseDto.getInternalInventoryDate()));
        return record;
    }

    public static FamilyRecord getFamilyRecord(FamilyDto familyDto) {
        FamilyRecord record = new FamilyRecord();
        record.setId(familyDto.getId());
        record.setUrn(familyDto.getUrn());
        record.setCode(familyDto.getCode());
        record.setTitle(getLocalisedString(familyDto.getTitle()));
        record.setProcStatus(CommonUtils.getProcStatusName(familyDto.getProcStatus()));
        record.setAcronym(getLocalisedString(familyDto.getAcronym()));
        record.setCreatedDate(DateUtils.getFormattedDate(familyDto.getCreatedDate()));
        record.setInternalInventoryDate(DateUtils.getFormattedDate(familyDto.getInternalInventoryDate()));
        return record;
    }

    /**
     * Returns {@link OperationRecord} from {@link OperationBaseDto}
     * 
     * @param operationBaseDto
     * @return
     */
    public static OperationRecord getOperationRecord(OperationBaseDto operationBaseDto) {
        OperationRecord record = new OperationRecord();
        record.setId(operationBaseDto.getId());
        record.setCode(operationBaseDto.getCode());
        record.setUrn(operationBaseDto.getUrn());
        record.setTitle(getLocalisedString(operationBaseDto.getTitle()));
        record.setAcronym(getLocalisedString(operationBaseDto.getAcronym()));
        record.setSubjectArea(operationBaseDto.getSubjectArea());
        record.setSurveyType(operationBaseDto.getSurveyType());
        record.setOfficialityType(operationBaseDto.getOfficialityType());
        record.setIndicatorsSystem(operationBaseDto.getIndicatorSystem());
        record.setCreatedDate(DateUtils.getFormattedDate(operationBaseDto.getCreatedDate()));
        record.setInternalInventoryDate(DateUtils.getFormattedDate(operationBaseDto.getInternalInventoryDate()));
        record.setCurrentlyActive(CommonWebUtils.getBooleanValueAsString(operationBaseDto.getCurrentlyActive()));
        record.setProcStatus(CommonUtils.getProcStatusName(operationBaseDto.getProcStatus()));
        record.setStatus(CommonUtils.getStatusName(operationBaseDto.getStatus()));
        record.setPublicationStreamStatus(operationBaseDto.getStreamMessageStatus());
        record.setOperationBaseDto(operationBaseDto);
        return record;
    }

    /**
     * Returns {@link InstanceRecord} from {@link InstanceBaseDto}
     * 
     * @param instanceDto
     * @return
     */
    public static InstanceRecord getInstanceRecord(InstanceBaseDto instanceDto) {
        InstanceRecord record = new InstanceRecord();
        record.setId(instanceDto.getId());
        record.setCode(instanceDto.getCode());
        record.setUrn(instanceDto.getUrn());
        record.setTitle(getLocalisedString(instanceDto.getTitle()));
        record.setAcronym(getLocalisedString(instanceDto.getAcronym()));
        record.setInstaceType(
                instanceDto.getInstanceType() != null ? CommonWebUtils.getElementName(instanceDto.getInstanceType().getIdentifier(), instanceDto.getInstanceType().getDescription()) : null);
        record.setCreatedDate(DateUtils.getFormattedDate(instanceDto.getCreatedDate()));
        record.setInternalInventoryDate(DateUtils.getFormattedDate(instanceDto.getInternalInventoryDate()));
        record.setProcStatus(CommonUtils.getProcStatusName(instanceDto.getProcStatus()));
        record.setOrder(instanceDto.getOrder());
        return record;
    }

    /**
     * Returns {@link InstanceRecord} from {@link InstanceDto}
     * 
     * @param instanceDto
     * @return
     */
    public static InstanceRecord getInstanceRecord(InstanceDto instanceDto) {
        InstanceRecord record = new InstanceRecord();
        record.setId(instanceDto.getId());
        record.setCode(instanceDto.getCode());
        record.setUrn(instanceDto.getUrn());
        record.setTitle(getLocalisedString(instanceDto.getTitle()));
        record.setAcronym(getLocalisedString(instanceDto.getAcronym()));
        record.setInstaceType(
                instanceDto.getInstanceType() != null ? CommonWebUtils.getElementName(instanceDto.getInstanceType().getIdentifier(), instanceDto.getInstanceType().getDescription()) : null);
        record.setCreatedDate(DateUtils.getFormattedDate(instanceDto.getCreatedDate()));
        record.setInternalInventoryDate(DateUtils.getFormattedDate(instanceDto.getInternalInventoryDate()));
        record.setProcStatus(CommonUtils.getProcStatusName(instanceDto.getProcStatus()));
        record.setOrder(instanceDto.getOrder());
        return record;
    }
}
