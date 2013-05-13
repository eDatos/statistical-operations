package org.siemac.metamac.statistical.operations.web.client.utils;

import static org.siemac.metamac.statistical.operations.web.client.OperationsWeb.getCoreMessages;
import static org.siemac.metamac.web.common.client.utils.InternationalStringUtils.getLocalisedString;

import org.siemac.metamac.statistical.operations.core.dto.FamilyBaseDto;
import org.siemac.metamac.statistical.operations.core.dto.FamilyDto;
import org.siemac.metamac.statistical.operations.core.dto.InstanceBaseDto;
import org.siemac.metamac.statistical.operations.core.dto.InstanceDto;
import org.siemac.metamac.statistical.operations.core.dto.OperationBaseDto;
import org.siemac.metamac.statistical.operations.core.dto.OperationDto;
import org.siemac.metamac.statistical.operations.web.client.model.FamilyRecord;
import org.siemac.metamac.statistical.operations.web.client.model.InstanceRecord;
import org.siemac.metamac.statistical.operations.web.client.model.OperationRecord;
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
        OperationRecord record = new OperationRecord(operationBaseDto.getId(), operationBaseDto.getCode(), getLocalisedString(operationBaseDto.getTitle()),
                getLocalisedString(operationBaseDto.getAcronym()), getCoreMessages().getString(getCoreMessages().procStatusEnum() + operationBaseDto.getProcStatus().getName()),
                operationBaseDto.getIndicatorSystem(), operationBaseDto.getSubjectArea(), operationBaseDto.getSurveyType(), operationBaseDto.getOfficialityType());
        return record;
    }
    /**
     * Returns {@link OperationRecord} from {@link OperationDto}
     * 
     * @param operationDto
     * @return
     */
    public static OperationRecord getOperationRecord(OperationDto operationDto) {
        OperationRecord record = new OperationRecord(operationDto.getId(), operationDto.getCode(), getLocalisedString(operationDto.getTitle()), getLocalisedString(operationDto.getAcronym()),
                getCoreMessages().getString(getCoreMessages().procStatusEnum() + operationDto.getProcStatus().getName()), operationDto.getIndicatorSystem(), operationDto.getSubjectArea(),
                operationDto.getSurveyType(), operationDto.getOfficialityType());
        return record;
    }

    /**
     * Returns {@link InstanceRecord} from {@link InstanceBaseDto}
     * 
     * @param instanceDto
     * @return
     */
    public static InstanceRecord getInstanceRecord(InstanceBaseDto instanceDto) {
        InstanceRecord record = new InstanceRecord(instanceDto.getId(), instanceDto.getCode(), getLocalisedString(instanceDto.getTitle()), getLocalisedString(instanceDto.getDataDescription()),
                getCoreMessages().getString(getCoreMessages().procStatusEnum() + instanceDto.getProcStatus().getName()), instanceDto.getOrder());
        return record;
    }

    /**
     * Returns {@link InstanceRecord} from {@link InstanceDto}
     * 
     * @param instanceDto
     * @return
     */
    public static InstanceRecord getInstanceRecord(InstanceDto instanceDto) {
        InstanceRecord record = new InstanceRecord(instanceDto.getId(), instanceDto.getCode(), getLocalisedString(instanceDto.getTitle()), null, getCoreMessages().getString(
                getCoreMessages().procStatusEnum() + instanceDto.getProcStatus().getName()), instanceDto.getOrder());
        return record;
    }

}
