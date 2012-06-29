package org.siemac.metamac.statistical.operations.web.client.utils;

import org.siemac.metamac.statistical.operations.core.dto.FamilyBaseDto;
import org.siemac.metamac.statistical.operations.core.dto.FamilyDto;
import org.siemac.metamac.statistical.operations.core.dto.InstanceBaseDto;
import org.siemac.metamac.statistical.operations.core.dto.InstanceDto;
import org.siemac.metamac.statistical.operations.core.dto.OperationBaseDto;
import org.siemac.metamac.statistical.operations.core.dto.OperationDto;
import org.siemac.metamac.statistical.operations.web.client.OperationsWeb;
import org.siemac.metamac.statistical.operations.web.client.model.FamilyRecord;
import org.siemac.metamac.statistical.operations.web.client.model.InstanceRecord;
import org.siemac.metamac.statistical.operations.web.client.model.OperationRecord;
import org.siemac.metamac.web.common.client.utils.InternationalStringUtils;

public class RecordUtils {

    /**
     * Returns {@link FamilyRecord} from {@link FamilyBaseDto}
     * 
     * @param familyBaseDto
     * @return
     */
    public static FamilyRecord getFamilyRecord(FamilyBaseDto familyBaseDto) {
        FamilyRecord record = new FamilyRecord(familyBaseDto.getId(), familyBaseDto.getCode(), InternationalStringUtils.getLocalisedString(familyBaseDto.getTitle()),
                InternationalStringUtils.getLocalisedString(familyBaseDto.getDescription()), OperationsWeb.getCoreMessages().getString(
                        OperationsWeb.getCoreMessages().procStatusEnum() + familyBaseDto.getProcStatus().getName()));
        return record;
    }

    /**
     * Returns {@link FamilyRecord} from {@link FamilyDto}
     * 
     * @param familyDto
     * @return
     */
    public static FamilyRecord getFamilyRecord(FamilyDto familyDto) {
        FamilyRecord record = new FamilyRecord(familyDto.getId(), familyDto.getCode(), InternationalStringUtils.getLocalisedString(familyDto.getTitle()),
                InternationalStringUtils.getLocalisedString(familyDto.getDescription()), OperationsWeb.getCoreMessages().getString(
                        OperationsWeb.getCoreMessages().procStatusEnum() + familyDto.getProcStatus().getName()));
        return record;
    }

    /**
     * Returns {@link OperationRecord} from {@link OperationBaseDto}
     * 
     * @param operationBaseDto
     * @return
     */
    public static OperationRecord getOperationRecord(OperationBaseDto operationBaseDto) {
        OperationRecord record = new OperationRecord(operationBaseDto.getId(), operationBaseDto.getCode(), InternationalStringUtils.getLocalisedString(operationBaseDto.getTitle()),
                InternationalStringUtils.getLocalisedString(operationBaseDto.getAcronym()), OperationsWeb.getCoreMessages().getString(
                        OperationsWeb.getCoreMessages().procStatusEnum() + operationBaseDto.getProcStatus().getName()), operationBaseDto.getIndicatorSystem());
        return record;
    }

    /**
     * Returns {@link OperationRecord} from {@link OperationDto}
     * 
     * @param operationDto
     * @return
     */
    public static OperationRecord getOperationRecord(OperationDto operationDto) {
        OperationRecord record = new OperationRecord(operationDto.getId(), operationDto.getCode(), InternationalStringUtils.getLocalisedString(operationDto.getTitle()),
                InternationalStringUtils.getLocalisedString(operationDto.getAcronym()), OperationsWeb.getCoreMessages().getString(
                        OperationsWeb.getCoreMessages().procStatusEnum() + operationDto.getProcStatus().getName()), operationDto.getIndicatorSystem());
        return record;
    }

    /**
     * Returns {@link InstanceRecord} from {@link InstanceBaseDto}
     * 
     * @param instanceDto
     * @return
     */
    public static InstanceRecord getInstanceRecord(InstanceBaseDto instanceDto) {
        InstanceRecord record = new InstanceRecord(instanceDto.getId(), instanceDto.getCode(), InternationalStringUtils.getLocalisedString(instanceDto.getTitle()),
                InternationalStringUtils.getLocalisedString(instanceDto.getDescription()), OperationsWeb.getCoreMessages().getString(
                        OperationsWeb.getCoreMessages().procStatusEnum() + instanceDto.getProcStatus().getName()), instanceDto.getOrder());
        return record;
    }

    /**
     * Returns {@link InstanceRecord} from {@link InstanceDto}
     * 
     * @param instanceDto
     * @return
     */
    public static InstanceRecord getInstanceRecord(InstanceDto instanceDto) {
        InstanceRecord record = new InstanceRecord(instanceDto.getId(), instanceDto.getCode(), InternationalStringUtils.getLocalisedString(instanceDto.getTitle()), null, OperationsWeb
                .getCoreMessages().getString(OperationsWeb.getCoreMessages().procStatusEnum() + instanceDto.getProcStatus().getName()), instanceDto.getOrder());
        return record;
    }

}
