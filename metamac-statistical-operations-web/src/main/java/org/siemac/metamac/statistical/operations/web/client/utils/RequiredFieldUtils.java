package org.siemac.metamac.statistical.operations.web.client.utils;

import org.siemac.metamac.statistical.operations.core.enume.domain.ProcStatusEnum;
import org.siemac.metamac.statistical.operations.web.client.model.ds.InstanceDS;
import org.siemac.metamac.statistical.operations.web.client.model.ds.OperationDS;

/**
 * This class contain methods that, given a procStatus of a resource, return the fields that are required to go to the next procStatus.
 */
public class RequiredFieldUtils {

    //
    // FAMILIES
    //

    private static final String[] familyFieldsToInternalPublication = new String[]{};
    private static final String[] familyFieldsToExternalPublication = familyFieldsToInternalPublication;

    public static String[] getFamilyRequiredFieldsToNextProcStatus(ProcStatusEnum currentProcStatus) {
        switch (currentProcStatus) {
            case DRAFT:
                return familyFieldsToInternalPublication;
            case PUBLISH_INTERNALLY:
                return familyFieldsToExternalPublication;
            default:
                return new String[]{};
        }
    }

    //
    // OPERATIONS
    //

    private static final String[] operationFieldsToInternalPublication = new String[]{OperationDS.OBJECTIVE, OperationDS.STATISTICAL_OPERATION_TYPE, OperationDS.OFFICIALITY_TYPE,
            OperationDS.PRODUCER, OperationDS.REG_RESPONSIBLE, OperationDS.CURRENTLY_ACTIVE, OperationDS.STATUS, OperationDS.PUBLISHER, OperationDS.COMMON_METADATA};
    private static final String[] operationFieldsToExternalPublication = operationFieldsToInternalPublication;

    public static String[] getOperationRequiredFieldsToNextProcStatus(ProcStatusEnum currentProcStatus) {
        switch (currentProcStatus) {
            case DRAFT:
                return operationFieldsToInternalPublication;
            case PUBLISH_INTERNALLY:
                return operationFieldsToExternalPublication;
            default:
                return new String[]{};
        }
    }

    private static final String[] instanceFieldsToInternalPublication = new String[]{InstanceDS.INSTANCE_TYPE};
    private static final String[] instanceFieldsToExternalPublication = instanceFieldsToInternalPublication;

    public static String[] getInstanceRequiredFieldsToNextProcStatus(ProcStatusEnum currentProcStatus) {
        switch (currentProcStatus) {
            case DRAFT:
                return instanceFieldsToInternalPublication;
            case PUBLISH_INTERNALLY:
                return instanceFieldsToExternalPublication;
            default:
                return new String[]{};
        }
    }
}
